suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr)} )

ge <- read.csv("^Data/gws_events.csv")
gem <- read.csv("^Data/gws_migr_events.csv")

#################################################################################################
#################################################################################################

#### CONTROLLARE BENE!!
#### SEMBRA SBAGLIATO

# Mean value 1-year
gem <- gem %>%
  arrange(type, orig, year) %>%
  group_by(orig, type) %>%
  mutate(mvalue1 = (value + lag(value, default = 0)) / 2)
# Mean conflicts 1-year
gem <- gem %>%
  arrange(type, orig, year) %>%
  group_by(orig, type) %>%
  mutate(mconfl1 = (conflicts + lag(conflicts, default = 0)) / 2)
# Mean deaths 1-year
gem <- gem %>%
  arrange(type, orig, year) %>%
  group_by(orig, type) %>%
  mutate(mdeaths1 = (deaths + lag(deaths, default = 0)) / 2)
# Variation value 1-year
gem <- gem %>%
  arrange(type, orig, year) %>%
  group_by(orig, type) %>%
  mutate(vvalue1 = (value - lag(value, default = 0)))
# Variation conflicts 1-year
gem <- gem %>%
  arrange(type, orig, year) %>%
  group_by(orig, type) %>%
  mutate(vconfl1 = (conflicts - lag(conflicts, default = 0)))

# Mean value 5-year
gem <- gem[order(gem$orig, gem$type, gem$year), ]
gem <- gem %>%
  group_by(orig, type) %>%
  mutate(mvalue5 = rollmean(value, k = 5, align = "right", fill = NA))
# Mean conflicts 5-year
gem <- gem[order(gem$orig, gem$type, gem$year), ]
gem <- gem %>%
  group_by(orig, type) %>%
  mutate(mconfl5 = rollmean(conflicts, k = 5, align = "right", fill = NA))
# Mean deaths 5-year
gem <- gem[order(gem$orig, gem$type, gem$year), ]
gem <- gem %>%
  group_by(orig, type) %>%
  mutate(mdeaths5 = rollmean(deaths, k = 5, align = "right", fill = NA))
# Variation value 5-years
gem <- gem[order(gem$orig, gem$type, gem$year), ]
gem <- gem %>%
  group_by(orig, type) %>%
  mutate(vvalue5 = value - lag(value, n = 4))
# Variation conflict 5-years
gem <- gem[order(gem$orig, gem$type, gem$year), ]
gem <- gem %>%
  group_by(orig, type) %>%
  mutate(vconfl5 = conflicts - lag(conflicts, n = 4))

















