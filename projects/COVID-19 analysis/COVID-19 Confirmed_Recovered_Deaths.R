library(tidyverse)
library(lubridate)
library(rvest)
library(patchwork)
library(leaflet)
library(countrycode)
library(plotly)

## --- DATA PULLING AND PROCESSING ----

# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
# Confirmed,deaths, and recovered will be pulled in the same method.
confirmed <- read.csv("time_series_19-covid-Confirmed.csv")
# Labels the data frame with cases corresponding to lines and variables to fields noted.
# Pivot the dates to line up by country through matching, rename the "new" date column to dates
# and move the data in each cell to n_STATUS.
# Mutate months to appropriate format.
# Paste 20 at the start of the year.
# Utililze lubridate to put dashes between the MM-DD-YYYY.
# Select variables listed through dplyr.
?separate
confirmed <- read.csv("time_series_19-covid-Confirmed.csv") %>%
  pivot_longer(cols = matches("^X[0-9]{1,2}"), names_to = "date", values_to = "n_confirmed") %>%
  separate(date, c("month", "day", "year"), sep = "\\.") %>%
  mutate(month = gsub("X", "", month),
         year = paste0("20", year),
         date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
  dplyr::select(Country.Region, Province.State, Lat, Long, date, n_confirmed)

deaths <- read.csv("time_series_19-covid-Deaths.csv") %>%
  pivot_longer(cols = matches("^X[0-9]{1,2}"), names_to = "date", values_to = "n_deaths") %>%
  separate(date, c("month", "day", "year"), sep = "\\.") %>%
  mutate(month = gsub("X", "", month),
         year = paste0("20", year),
         date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
  dplyr::select(Country.Region, Province.State, Lat, Long, date, n_deaths)

recovered <- read.csv("time_series_19-covid-Recovered.csv") %>%
  pivot_longer(cols = matches("^X[0-9]{1,2}"), names_to = "date", values_to = "n_recovered") %>%
  separate(date, c("month", "day", "year"), sep = "\\.") %>%
  mutate(month = gsub("X", "", month),
         year = paste0("20", year),
         date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
  dplyr::select(Country.Region, Province.State, Lat, Long, date, n_recovered)

#Merge all three lists into one list with confirmed, deaths, and recovered.
full <- Reduce(function(...) merge(..., all = TRUE), list(confirmed, deaths, recovered))

global <- full %>%
  group_by(date) %>%
  summarize_at(vars(starts_with("n_")), sum)

global_change <- global %>%
  mutate_at(vars(starts_with("n_")), function(x) x - lag(x)) %>%
  slice(-1) 

global %>%
  # recompute confirmed as net of deaths and recovered for stacked plotting
  mutate(n_confirmed = n_confirmed - n_deaths - n_recovered) %>%
  pivot_longer(-date, names_to = "casetype", values_to = "n") %>%
  mutate(casetype = gsub("n_", "", casetype)) %>%
  ggplot(aes(x = date, y = n, fill = casetype)) +
  geom_col(alpha = 2/3) +
  theme_minimal() +
  scale_fill_manual(values = c("gray75", "red", "dodgerblue")) +
  labs(caption = "Source: Johns Hopkins University Dataset") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom")

global_change %>%
  # recompute confirmed as net of deaths and recovered for stacked plotting
  mutate(n_confirmed = n_confirmed - n_deaths - n_recovered) %>%
  pivot_longer(-date, names_to = "casetype", values_to = "n") %>%
  mutate(casetype = gsub("n_", "", casetype)) %>%
  ggplot(aes(x = date, y = n, fill = casetype)) +
  geom_col(alpha = 2/3) +
  theme_minimal() +
  scale_fill_manual(values = c("gray75", "red", "dodgerblue")) +
  labs(caption = "Source: Johns Hopkins University Dataset") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom")

