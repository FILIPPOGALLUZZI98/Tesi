suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

shp <- st_read("^Data/shp/shp.shp")
events <- read.csv("^Data/Global_events.csv")
data_gw <- read.csv("^Data/Global_gws.csv")
data_gw_events <-read.csv("^Data/Global_events_gws.csv")


data <- data_gw_events %>%
  group_by(country, region, year, value) %>%
  summarise(conflict = sum(number))

lm <- lm(data$conflict ~ data$value + as.factor(data$year) + as.factor(data$region))
summary(lm)

