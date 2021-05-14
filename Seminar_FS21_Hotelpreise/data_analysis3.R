
# Intro -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(beepr)
library(DataCombine)
library(extrafont) 
library(ggthemes)
library(scales)
library(egg)


# Datenimport -------------------------------------------------------------

# disable scientif notation
options(scipen = 999)

# font_import(pattern = "lmroman*") # first time use
theme_set(theme_bw() %+replace%
            theme(text = element_text(family = "LM Roman 10")))

date_start <- ymd("2021-03-18")
date_today <- ymd(Sys.Date())
setwd("./files")
file_names <- list.files(pattern = "*.csv")

# Read all data
f_read_csv <- function(file_name) {
  df_temp <- read_csv(file_name, col_types = cols())
  print(paste("Daten von", file_name, "sind eingelesen."))
  return(df_temp)
}

data_raw <- map_dfr(file_names, f_read_csv)
beep(3)

# import hotel locations from different scrap
data_locations_raw <- read_csv("hotel_locations/scrap_data_w_hotelnames2021-04-29.csv")

setwd("../..")


# Datenaufbereitung -------------------------------------------------------

head(data_raw) %>% select(name, price)

data <- data_raw %>% 
  mutate(name = str_remove_all(name, "\n"),
         checkin = date_checkin - scrap_date,
         checkout = date_checkout - scrap_date,
         nights = as.numeric(date_checkout-date_checkin),
         cat_date = str_c(checkin, checkout, sep = "-"),
         cat_pers = str_c(adults, children, rooms, sep = "-"),
         price = as.numeric(str_extract_all(str_remove_all(data_raw$price, ","), "[0-9]+")),
         price_night = price / nights) 

head(data,50) %>% 
  select(name, price_night)

# data %>% select(town) %>% unique() %>% view

# replacement matrix for misspelled and mistransformed townnames
# same for hotel locations

d_replacememnt <- tribble(
  ~pattern, ~replacement,
  "Ch<c3><a2>ble Verbier", "Châble Verbier",
  "Anz<c3><a8>re", "Anzère",
  "Z<c3><bc>rich", "Zürich",
  "ChÃ¢ble Verbier", "Châble Verbier",
  "AnzÃ¨re", "Anzère",
  "ZÃ¼rich", "Zürich",
  "ChÃ¢ble Verbier", "Châble Verbier",
  "LeChâble", "Châble Verbier",
  "SanktMoritz", "St. Moritz",
  "St.Moritz", "St. Moritz",
  "DavosWiesen", "Davos Wiesen",
  "KlostersSerneus", "Klosters Serneus",
  "SertigDöfliDavos", "Sertig Döfli Davos",
  "KlostersDorf", "Klosters Dorf",
  "DavosDorf", "Davos Dorf",
  "LesDiablerets", "Les Diablerets",
  "ZÃ¼rich", "Zürich",
  "Geneva", "Genf",
  "Lucerne", "Luzern"
)

data <- FindReplace(data = as.data.frame(data),
                    Var = "town",
                    replaceData = d_replacememnt,
                    from = "pattern",
                    to = "replacement",
                    exact = FALSE)


# add hotel locations
data_locations <- data_locations_raw %>%
  filter(tag != "NA") %>%
  select(name, tag, town) %>%
  unique()

data_locations <- data_locations %>% 
  mutate(name = str_remove_all(name, "\n"),
         town1 = str_remove_all(tag, "\n"),
         town1 = str_remove_all(town1, "Show on map"),
         town1 = str_remove_all(town1, " "))

# correct misspelled towns
data_locations <- FindReplace(data = as.data.frame(data_locations),
                              Var = "town",
                              replaceData = d_replacememnt,
                              from = "pattern",
                              to = "replacement",
                              exact = FALSE)
data_locations <- FindReplace(data = as.data.frame(data_locations),
                              Var = "town1",
                              replaceData = d_replacememnt,
                              from = "pattern",
                              to = "replacement",
                              exact = FALSE)

# towncheck
data_locations <- data_locations %>% 
  mutate(test_orig = str_replace_all(town, "-", " "),
         test_orig = str_replace_all(test_orig, "/", " "),
         test_orig = str_replace_all(test_orig, ",", " "),
         test_scrap = str_replace_all(town1, "-", " "),
         test_scrap = str_replace_all(test_scrap, "/", " "),
         test_scrap = str_replace_all(test_scrap, ",", " "),
         test1 = str_detect(test_scrap, test_orig),
         test2 = str_detect(test_orig, test_scrap),
         test_loc = ifelse(test1 == TRUE | test2 == TRUE, TRUE, FALSE))

hotels <- data_locations %>% 
  select(name, town, town1, test_loc)

d_all <- left_join(data, hotels)

# add town groups, same as in scraping file

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


d_all <- d_all %>% 
  mutate(cat_town = ifelse(town %in% towns_ml$town, "ml",
                           ifelse(town %in% towns_mf$town, "mf",
                                  ifelse(town %in% towns_bc$town, "bc",
                                         NA))),
         cat_town = factor(cat_town, c("ml", "mf", "bc")),
         town = factor(town, ordered = FALSE),
         price_night = price/nights,
         price_night_room = price_night/rooms)

d <- d_all %>%
  filter(test_loc == TRUE)

print(paste(object.size(d_all)/1000000, "MB"))
print(paste(object.size(d)/1000000, "MB"))

beep(6)

# Bundesratsentscheide
date_br_an_1 <- dmy("19-3-2021")
date_br_op_1 <- dmy("22-3-2021")
date_br_an_2 <- dmy("14-4-2021")
date_br_op_2 <- dmy("19-4-2021")
date_br = c(date_br_an_1, date_br_op_1, date_br_an_2, date_br_op_2)







# Analysen ----------------------------------------------------------------


# Table 1 - Summary -------------------------------------------------------
d %>%
  group_by(cat_town) %>%
  summarise("Min." = round(min(price_night_room)), 
            "1st Qu." = round(quantile(price_night_room, 0.25)), 
            "Median" = round(quantile(price_night_room, 0.5)), 
            "Mean" = round(mean(price_night_room)), 
            "3rd Qu." = round(quantile(price_night_room, 0.75)), 
            "Max." = round(max(price_night_room)), 
            "NA's" = round(length(is.na(price_night_room)))) %>%
  knitr::kable(format = "latex")

# barplot mit anzahl hotels pro stadt -------------------------------------
town_sort <- d %>%
  group_by(town, cat_town) %>%
  summarise(town_hotels = length(unique(name))) %>%
  arrange(-town_hotels) %>%
  select(town)
town_sort <- town_sort$town

d %>%
  group_by(town, cat_town) %>%
  summarise(town_hotels = length(unique(name))) %>%
  mutate(town = factor(town, town_sort)) %>%
  ggplot() +
  geom_col(aes(town, town_hotels, fill = cat_town)) +
  labs(x = "Ortschaft", y = "Anzahl Hotels & Apartments") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top") +
  scale_fill_economist(name = "Kategorie:", labels = c("Bergdorf gross", "Bergdorf klein", "Grossstadt"))

ggsave("paper/barplot_town_hotels.png", scale = 1, width = 20, height = 10, units = "cm")


# Varianz und Median der Hotelpreisübernachtungen nach Städten -----------------------

p_median <- d %>%
  filter(cat_date == "0-2" &
           cat_town == "bc") %>%
  group_by(scrap_date, town) %>%
  mutate(price_median_night = median(price_night_room)) %>%
  ggplot() +
  geom_line(aes(scrap_date, price_median_night, color = town)) +
  # geom_smooth(aes(scrap_date, price_median_night, color = town), se = FALSE) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  labs(x = "Datum", y = "Median (CHF)", title = "Ganzer Zeitraum") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d.%m") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  scale_color_economist()

p_var <- d %>%
  filter(cat_date == "0-2" & cat_town == "bc") %>%
  group_by(scrap_date, town) %>%
  mutate(price_var_night = var(price_night_room)) %>%
  ggplot() +
  geom_line(aes(scrap_date, price_var_night, color = town)) +
  # geom_smooth(aes(scrap_date, price_var_night, color = town), se = FALSE) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  labs(x = "Anreisedatum", y = "Varianz (CHF)") +
  theme(legend.position = "none") +
  scale_color_economist(name = "Stadt:") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d.%m")

p_median_var <- egg::ggarrange(p_median, p_var, ncol = 1)
ggsave("paper/ts_town_var.png", plot = p_median_var, scale = 1, width = 20, height = 15, units = "cm")



# Zoom in one week before and after
p_median_week <- d %>%
  filter(cat_date == "0-2" &
           cat_town == "bc" &
           date_br[4] >= scrap_date - weeks(1) &
           date_br[3] <= scrap_date + weeks(1)) %>%
  group_by(scrap_date, town) %>%
  mutate(price_median_night = median(price_night_room)) %>%
  ggplot() +
  geom_line(aes(scrap_date, price_median_night, color = town)) +
  # geom_smooth(aes(scrap_date, price_median_night, color = town), se = FALSE) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  labs(x = "Datum", y = "Median (CHF)", title = "Fokus auf Bundesratsentscheid") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d.%m") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  scale_color_economist()

p_var_week <- d %>%
  filter(cat_date == "0-2" & 
           cat_town == "bc" &
           date_br[4] >= scrap_date - weeks(1) &
           date_br[3] <= scrap_date + weeks(1)) %>%
  group_by(scrap_date, town) %>%
  mutate(price_var_night = var(price_night_room)) %>%
  ggplot() +
  geom_line(aes(scrap_date, price_var_night, color = town)) +
  # geom_smooth(aes(scrap_date, price_var_night, color = town), se = FALSE) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  labs(x = "Anreisedatum", y = "Varianz (CHF)") +
  theme(legend.position = "bottom") +
  scale_color_economist(name = "Stadt:") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d.%m")

p_median_var_week <- egg::ggarrange(p_median_week, p_var_week, ncol = 1)
ggsave("paper/ts_town_var_week.png", plot = p_median_var_week, scale = 1, width = 20, height = 15, units = "cm")


p_median_var_all <- egg::ggarrange(p_median, p_var, p_median_week, p_var_week, ncol = 1)
ggsave("paper/ts_town_var_all.png", plot = p_median_var_all, scale = 1, width = 20, height = 30, units = "cm")


# loess - Varianz und Median der Hotelpreisübernachtungen nach Städten -----------------------

p_median_loess <- d %>%
  filter(cat_date == "0-2" &
           cat_town == "bc") %>%
  group_by(scrap_date, town) %>%
  mutate(price_median_night = median(price_night_room)) %>%
  ggplot() +
  # geom_line(aes(scrap_date, price_median_night, color = town)) +
  geom_smooth(aes(scrap_date, price_median_night, color = town), method = "loess", se = FALSE) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  labs(x = "Datum", y = "Median (CHF)", title = "Ganzer Zeitraum") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d.%m") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  scale_color_economist()

p_var_loess <- d %>%
  filter(cat_date == "0-2" & cat_town == "bc") %>%
  group_by(scrap_date, town) %>%
  mutate(price_var_night = var(price_night_room)) %>%
  ggplot() +
  # geom_line(aes(scrap_date, price_var_night, color = town)) +
  geom_smooth(aes(scrap_date, price_var_night, color = town), method = "loess", se = FALSE) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  labs(x = "Anreisedatum", y = "Varianz (CHF)") +
  theme(legend.position = "none") +
  scale_color_economist(name = "Stadt:") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d.%m")

p_median_var_loess <- egg::ggarrange(p_median_loess, p_var_loess, ncol = 1)
ggsave("paper/ts_town_var_loess.png", plot = p_median_var_loess, scale = 1, width = 20, height = 15, units = "cm")



# Zoom in one week before and after
p_median_week_loess <- d %>%
  filter(cat_date == "0-2" &
           cat_town == "bc" &
           date_br[4] >= scrap_date - weeks(1) &
           date_br[3] <= scrap_date + weeks(1)) %>%
  group_by(scrap_date, town) %>%
  mutate(price_median_night = median(price_night_room)) %>%
  ggplot() +
  # geom_line(aes(scrap_date, price_median_night, color = town)) +
  geom_smooth(aes(scrap_date, price_median_night, color = town), method = "loess", se = FALSE) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  labs(x = "Datum", y = "Median (CHF)", title = "Fokus auf Bundesratsentscheid") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d.%m") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  scale_color_economist()

p_var_week_loess <- d %>%
  filter(cat_date == "0-2" & 
           cat_town == "bc" &
           date_br[4] >= scrap_date - weeks(1) &
           date_br[3] <= scrap_date + weeks(1)) %>%
  group_by(scrap_date, town) %>%
  mutate(price_var_night = var(price_night_room)) %>%
  ggplot() +
  # geom_line(aes(scrap_date, price_var_night, color = town)) +
  geom_smooth(aes(scrap_date, price_var_night, color = town), method = "loess", se = FALSE) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  labs(x = "Anreisedatum", y = "Varianz (CHF)") +
  theme(legend.position = "bottom") +
  scale_color_economist(name = "Stadt:") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d.%m")

p_median_var_week_loess <- egg::ggarrange(p_median_week_loess, p_var_week_loess, ncol = 1)
ggsave("paper/ts_town_var_week_loess.png", plot = p_median_var_week_loess, scale = 1, width = 20, height = 15, units = "cm")


p_median_var_all_loess <- egg::ggarrange(p_median_loess, p_var_loess, p_median_week_loess, p_var_week_loess, ncol = 1)
ggsave("paper/ts_town_var_all_loess.png", plot = p_median_var_all_loess, scale = 1, width = 20, height = 30, units = "cm")



# Fallstudie Murten -------------------------------------------------------

p_murten <- d %>%
  filter(cat_date == "90-104" & cat_town == "bc") %>%
  group_by(scrap_date, town, 
           # cat_date
           ) %>%
  mutate(price_mean = median(price_night),
         cat_date) %>%
  ggplot() +
  geom_line(aes(scrap_date, price_mean, color = town, size = town)) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  # facet_wrap(.~cat_date, ncol = 5) +
  labs(x = "Buchungsdatum", y = "Median (CHF)") +
  theme(legend.position = "bottom") +
  scale_color_economist(name = "Ortschaft:") +
  scale_size_manual(values = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 2, 0.5, 0.5)) +
  guides(size = FALSE)
p_murten

ggsave("paper/ts_murten.png", plot = p_murten, scale = 1, width = 20, height = 10, units = "cm")




# Fallstudie Leukerbad -------------------------------------------------------

p_leukerbad <- d %>%
  filter(cat_date == "0-2" & cat_town == "mf") %>%
  group_by(scrap_date, town, 
           # cat_date
           ) %>%
  mutate(price_mean = median(price_night),
         cat_date) %>%
  ggplot() +
  geom_line(aes(scrap_date, price_mean, color = town, size = town)) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  # facet_wrap(.~cat_date, ncol = 5) +
  labs(x = "Buchungsdatum", y = "Median (CHF)") +
  theme(legend.position = "bottom") +
  scale_color_economist(name = "Ortschaft:") +
  scale_size_manual(values = c(0.5, 0.5, 0.5, 0.5, 2, 0.5, 0.5)) +
  guides(size = FALSE)
p_leukerbad
ggsave("paper/ts_leukerbad.png", plot = p_leukerbad, scale = 1, width = 20, height = 10, units = "cm")



# Fallstudie Arosa -------------------------------------------------------

p_arosa <- d %>%
  filter(cat_date == "90-97" & 
           cat_town == "ml" &
           town %in% head(unique(town), 9)) %>%
  group_by(scrap_date, town) %>%
  mutate(price_mean = median(price_night),
         cat_date) %>%
  ggplot() +
  geom_line(aes(scrap_date, price_mean, color = town, size = town)) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  # facet_wrap(.~cat_date, ncol = 5, scales = "free_y") +
  labs(x = "Buchungsdatum", y = "Median (CHF)") +
  theme(legend.position = "bottom") +
  scale_color_economist(name = "Ortschaft:") +
  scale_size_manual(values = c(2, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)) +
  guides(size = FALSE)
p_arosa
ggsave("paper/ts_arosa.png", plot = p_arosa, scale = 1, width = 20, height = 10, units = "cm")

# Sandkasten --------------------------------------------------------------
# Diese Plots dienen der explorativen Analyse und sind nicht weiter aussagekräftig

unique(d$name[d$town=="Leukerbad"])
unique(d$nights)

# d %>%
#   filter(town == "Leukerbad") %>%
#   group_by(name) %>%
#   summarise(price = mean(price_night)) %>%
#   view()


# barplot mit anzahl beobachtungen pro stadt
d %>%
  mutate(town = fct_rev(fct_infreq(town))) %>%
  ggplot() +
  geom_bar(aes(town, fill = cat_town)) +
  coord_flip() +
  labs(x = "Stadt", y = "Anzahl Beobachtungen", title = "Barplot") +
  scale_fill_economist(name = "Kategorie:", labels = c("Bergdorf gross", "Bergdorf klein", "Grossstadt"))




d %>%
  filter(cat_pers == "1-0-1" & price_night < 2000) %>%
  mutate(nights = factor(nights)) %>%
  ggplot() +
  geom_boxplot(aes(x = nights, y = price_night, color = town)) +
  facet_grid(cat_town ~ ., scales = "free_y") +
  labs(x = "Nächte", y = "Preis/Nacht", title = "Boxplot der Hotelübernachtungen") 
# scale_color_economist(name = "Kategorie:", labels = c("Bergdorf gross", "Bergdorf klein", "Grossstadt"))


d %>%
  filter(cat_pers == "1-0-1" &
           cat_town == "ml") %>%
  group_by(town, cat_town, nights) %>%
  summarise(price_night_median = median(price_night)) %>%
  ggplot() +
  geom_line(aes(x = nights, y = price_night_median, color = town)) +
  # facet_grid(cat_town ~ ., scales = "free_y") +
  labs(x = "Nächte", y = "Preis/Nacht", title = "Median der Hotelübernachtungen") +
  theme_minimal() +
  scale_color_economist(name = "Ortschaft:")

d %>%
  filter(cat_pers == "1-0-1" & nights <= 2) %>%
  group_by(town, cat_town, nights) %>%
  summarise(price_night_median = median(price_night)) %>%
  mutate(price_night_median_lag = lag(price_night_median),
         delta = price_night_median_lag - price_night_median) %>%
  group_by(town) %>%
  mutate(cat_price_delta = ifelse(delta > 0, "increase", "decrease")) %>%
  filter(!is.na(delta)) %>%
  select(town, cat_price_delta) %>% view()


d %>%
  filter(cat_pers == "1-0-1" & cat_date == "7-8" & cat_town == "bc") %>%
  group_by(scrap_date, town) %>%
  mutate(price_mean = mean(price)) %>%
  ggplot() +
  geom_line(aes(date_checkin, price_mean, color = town)) +
  labs(x = "Datum", y = "Preis", title = "Mittelwert der Hotelübernachtungen") +
  scale_color_economist(name = "Ortschaft:")


d %>%
  filter(cat_date == "90-104" & cat_town == "bc") %>%
  group_by(scrap_date, town) %>%
  mutate(price_mean = median(price_night)) %>%
  ggplot() +
  geom_line(aes(date_checkin, price_mean, color = town)) +
  facet_grid(. ~ cat_pers) +
  labs(x = "Datum", y = "Preis", title = "Mittelwert der Hotelübernachtungen") +
  scale_color_economist(name = "Ortschaft:")


d %>%
  filter(cat_date == "90-91" & cat_town == "bc" & town == "Murten") %>%
  group_by(scrap_date, name) %>%
  mutate(price_mean = median(price_night)) %>%
  ggplot() +
  geom_line(aes(date_checkin, price_mean, color = name)) +
  facet_grid(. ~ cat_pers) +
  labs(x = "Datum", y = "Preis", title = "Mittelwert der Hotelübernachtungen") +
  theme_minimal()  

d %>%
  filter(cat_date == "30-31") %>%
  group_by(scrap_date, cat_town) %>%
  mutate(price_mean = mean(price)) %>%
  ggplot() +
  geom_line(aes(date_checkin, price_mean/14, color = cat_town)) +
  facet_grid(. ~ cat_pers) +
  labs(x = "Datum", y = "Preis", title = "Mittelwert der Hotelübernachtungen") +
  scale_color_economist(name = "Kategorie:", labels = c("Bergdorf gross", "Bergdorf klein", "Grossstadt"))


d %>%
  mutate(cat_date_2 = factor(date_checkin - scrap_date)) %>%
  filter(date_checkout - date_checkin <= days(1)) %>%
  group_by(scrap_date, cat_date_2, nights) %>%
  mutate(price_mean_night = median(price/nights)) %>%
  ggplot() +
  geom_line(aes(scrap_date, price_mean_night, color = cat_date_2)) +
  geom_vline(aes(xintercept = date_br[1]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[2]), color = "black", linetype = 1) +
  geom_vline(aes(xintercept = date_br[3]), color = "black", linetype = 2) +
  geom_vline(aes(xintercept = date_br[4]), color = "black", linetype = 1) +
  labs(x = "Datum", y = "Preis", title = "Median der Hotelübernachtungen") +
  scale_color_economist(name = "Anreisedatum:", 
                        labels = c("Heute", 
                                   "in einer Woche", 
                                   "in zwei Wochen",
                                   "in einem Monat",
                                   "in drei Monaten",
                                   "in einem halben Jahr"))


d %>%
  mutate(cat_date_2 = factor(date_checkin - scrap_date),
         scrap_day = wday(scrap_date),
         price_night = price / nights) %>%
  # filter(date_checkout - date_checkin <= days(1)) %>%
  group_by(scrap_day, cat_date_2) %>%
  summarise(price_mean_night = median(price_night)) %>%
  ggplot() +
  geom_path(aes(scrap_day, price_mean_night, color = cat_date_2)) +
  # facet_grid(. ~ cat_pers) +
  labs(x = "Datum", y = "Preis", title = "Median der Hotelübernachtungen") 

d %>%
  filter(cat_date == "90-91" &
           cat_town == "bc" &
           date_br[2] >= scrap_date - weeks(1) &
           date_br[1] <= scrap_date + weeks(1)) %>%
  mutate(price_night = (price/nights)/rooms) %>%
  group_by(scrap_date, town, cat_pers) %>%
  mutate(price_mean_night = median(price_night)) %>%
  ggplot() +
  geom_line(aes(scrap_date, price_mean_night, color = town, linetype = cat_pers)) +
  geom_vline(aes(xintercept = date_br[1]), color = "red") +
  geom_vline(aes(xintercept = date_br[2]), color = "red") +
  labs(x = "Datum", y = "Median Preis pro Nacht pro Zimmer", title = "Median der Hotelübernachtungen") +
  scale_color_economist(name = "Stadt:")







