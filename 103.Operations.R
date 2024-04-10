suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr)} )

ge <- read.csv("^Data/gws_events.csv")
gemp <- read.csv("^Data/gws_migr_events.csv")


#################################################################################################
#################################################################################################
gem <- gemp


# Mean value 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mvalue1 = (lag(value) + value) / 2)
# Mean conflicts 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mconfl1 = (lag(conflicts) + conflicts) / 2)
# Mean deaths 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mdeaths1 = (lag(deaths) + deaths) / 2)











