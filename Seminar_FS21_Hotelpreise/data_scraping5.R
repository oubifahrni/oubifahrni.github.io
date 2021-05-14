
# Introduction ------------------------------------------------------------

# This script is run everyday from march 17th to may 12th at 6am. This script 
# is used in a Cron job which let it run by itself every morning. Package cronR 
# is used.

# script last significantly updated: 18.3
# script last updated: 13.5 (comments and cleaning)

# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(rvest)
library(curl)
library(tictoc)


# Parameters --------------------------------------------------------------

## Date 
date_today <- ymd(Sys.Date())

# checkin dates to check
date_checkin <- c(date_today,
                 date_today + 7,
                 date_today + 14,
                 date_today + 30,
                 date_today + 90,
                 date_today + 180)

# number of nights to check, create a tibble with all dates
dates <- tibble(date_checkin) %>%
  mutate(date_1 = date_checkin + 1,
         date_2 = date_checkin + 2,
         date_3 = date_checkin + 3,
         date_7 = date_checkin + 7,
         date_14 = date_checkin + 14
         )

# combine into a date-matrix
dates <- dates %>%
  gather(key = "key", value = "date_checkout", -date_checkin) %>%
  select(1,3)

## number of persons and rooms
persons <- tribble(
  ~adults, ~children, ~rooms,
  1, 0, 1,
  2, 0, 1,
  2, 2, 2
)

## Towns 
# mountain towns that are more luxury/expensive "ml"
# https://www.planetware.com/switzerland/top-rated-ski-resorts-in-switzerland-ch-1-2.htm
# https://www.snowpak.com/switzerland/best-ski-resorts
# town_id, town_type and n are collected manually from booking.com
towns_ml <- tribble(
  ~town, ~town_id, ~town_type, ~n,
  "Zermatt", "-2554901", "city", 91,
  "Grindelwald-Wengen", "900187756", "landmark", 142,
  "Châble Verbier", "252624", "landmark", 71,
  "St. Moritz", "900185594", "landmark", 85,
  "Davos Klosters", "3753", "region", 55,
  "Saas-Fee", "-2553924", "city", 18,
  "Flims Laax Falera", "3758", "region", 33,
  "Arosa Lenzerheide", "900187745", "landmark", 32,
  "Engelberg", "-2551893", "city", 22,
  "Adelboden Lenk", "900187741", "landmark", 29,
  "Andermatt Sedrun", "900187164", "landmark", 12,
  "Crans-Montana", "-2551709", "city", 33,
  "Gstaad Zweisimmen", "3734", "region", 37)

# mountain towns that are more family oriented/cheaper "mf"
# https://www.magicpass.ch/en/ski-resorts/
towns_mf <- tribble(
  ~town, ~town_id, ~town_type, ~n,
  "Anzère", "900040837", "city", 22,
  "Charmey", "-2551579", "city", 3,
  "Grimentz Zinal", "900126510", "landmark", 12,
  "Les Diablerets Villars Gryon", "900187169", "landmark", 45,
  "Leysin", "-2552905", "city", 8,
  "Leukerbad", "-2552898", "city", 26,
  "Ovronnaz", "-2553551", "city", 5)

# bigger cities "bc"
towns_bc <- tribble(
  ~town, ~town_id, ~town_type, ~n,
  "Zürich", "-2554935", "city", 156,
  "Bern", "-2551235", "city", 57,
  "Basel", "-2551183", "city", 84,
  "Genf", "-2552151", "city", 102,
  "Lausanne", "-2552809", "city", 47,
  "Luzern", "-2552994", "city", 90,
  "Winterthur", "-2554859", "city", 16,
  "Lugano", "-2552969", "city", 57,
  "Murten", "-2553283", "city", 12)

# merge into one dataframe and number of pages
towns <- bind_rows(towns_ml, towns_mf) %>%
  bind_rows(., towns_bc) %>%
  mutate(pages = ceiling(n/25),
         pn = 0) %>%
  slice(rep(1:n(), pages)) %>%
  group_by(town_id) %>%
  mutate(pn = (row_number()-1)*25)

## create a tibble withh all need combinations
parameters <- as_tibble(merge(dates, persons) %>%
  merge(., towns))


# Links for scraping ------------------------------------------------------

# construct link by towns, date, persons, rooms and pagenumber
f_link <- function(checkin, checkout, town_type, town_id, adults, children, rooms, pn) {
  link <- paste("https://www.booking.com/searchresults.en-gb.html?&tmpl=searchresults&ac_click_type=b&ac_position=0&checkin_month=", 
                month(checkin), "&checkin_monthday=", day(checkin), "&checkin_year=", year(checkin), 
                "&checkout_month=", month(checkout), "&checkout_monthday=", day(checkout), 
                "&checkout_year=", year(checkout), "&class_interval=1&dest_id=", town_id, 
                "&dest_type=", town_type, "&from_sf=1&group_adults=", adults, 
                "&group_children=", children, "&label_click=undef&no_rooms=", rooms, 
                "&sb_price_type=total&rows=25&offset=", pn, sep = "")
  return(link)
}

# add link to parameter tibble
links <- parameters %>%
  mutate(link = f_link(date_checkin, date_checkout, town_type, town_id, adults, children, rooms, pn),
         rn = row_number(link))


# Scraping Function -------------------------------------------------------

f_scrap <- function(link) {
  # load website
  page <- read_html(link)
  # get data out of page by html_nodes
  name <- page %>% html_nodes(".sr-hotel__name") %>% html_text()
  price <- page %>% html_nodes(".bui-price-display__value.prco-inline-block-maker-helper") %>% html_text()
  # check if same number of prices as names were scraped
  price <- if (length(price) == length(name)) {
    price
  } else {
    c(rep("NA", length(name)))
  }
  # add scrapping time and date
  df_temp <- tibble(link, name, price) %>%
    mutate(scrap_date = ymd(Sys.Date()),
           scrap_time = as_datetime(Sys.time()))
  # return scraped results
  return(df_temp)
  closeAllConnections()
}


# Scrap Data --------------------------------------------------------------
# create an empty list
d_list <- vector("list", length(links$link))
tic()
# 1st for-loop for scraping with failsafe
for (i in seq_along(links$link)) {
  tic()
  # check if link has already been scraped
  if (!(links$link[i] %in% names(d_list))) {
    cat(paste("Doing Link Nr.  ", i, " ...  ", sep = ""))
    ok <- FALSE
    counter <- 0
    # as long as it is not sucessfully scraped, scrap it counter-times,

    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({  
        # try to scrap link
        f_scrap(links$link[i])
      },
      error = function(e) {
        # wait 2 sec between trys
        Sys.sleep(2)
        e
      }
      )
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        time_taken <- toc(quiet = TRUE)
        ok <- TRUE
        cat(paste(" Done. (", round(time_taken$toc - time_taken$tic, 3), " seconds)"), sep = "")
      }
    }
    cat("\n")
    # save scraped data in list
    d_list[[i]] <- out
    names(d_list)[i] <- links$link[i]
  }
} 
time_taken <- toc(quiet = TRUE)
print(paste("All Links are scraped!! Good Job (", 
                        round((time_taken$toc - time_taken$tic)/3600, 1),
                        " hours)", sep = ""))

# create function to get data out of list and into one single df
f_merge <- function(i) {
  temp <- d_list[[i]]
  return(temp)
}

data_raw <- map_dfr(1:length(d_list), f_merge)

# combine with parameters defined before
export <- left_join(data_raw, links, by = "link")


# Split into Chunks - disregarded -------------------------------------------------------
# until failsafe above was implemented, I used to create chunks of links in between
# it waited a couple of seconds to scrap, did not solve the problem if an error occured.
# 
# 
# # set number of chunks
# chunks <- 10
# 
# # split links into chunks
# links_split <- split(links, 1:chunks)
# 
# Scrap Data
# 
# # create an empty tibble
# data_raw <- tribble(~link, ~name, ~price, ~scrap_date, ~scrap_time)
# 
# tic()
# # run through each chunk, find prices and pause it
# for (i in 1:chunks) {
#   tic()
#   
#   # scrap data for specific chunk
#   data_raw_temp <- map_dfr(links_split[[i]]$link, f_scrap)
#   
#   # bind scraped data to a general tibble
#   data_raw <- bind_rows(data_raw, data_raw_temp)
#   
#   # make scraping pause
#   Sys.sleep(60)
#   
#   # measure time it took for chunk
#   time_taken <- toc(quiet = TRUE)
#   print(paste("Chunk Nr. ", i, " is scraped!!  (", 
#               round((time_taken$toc - time_taken$tic)/60),
#               " minutes)", sep = ""))
# }
# time_taken <- toc(quiet = TRUE)
# print(paste("All Chunks are scraped!! Good Job (", 
#             round((time_taken$toc - time_taken$tic)/60),
#             " minutes)", sep = ""))
# 
# # add auxilary information (parameters and links) to scraped data
# export <- left_join(data_raw, links)
# 
# # method without chunk --> timeout error
# # tic()
# # data_raw <- map_dfr(links$link, f_scrap)
# # toc()

# Export Data as .csv -----------------------------------------------------

write_csv(export, paste("/Users/oubi/Documents/Bildung/University/MSc-Economics/Seminar - Tourismusforschung/Data/files/scrap_data_", ymd(Sys.Date()), ".csv", sep = ""))

print("Scraped data sucessfully saved")

