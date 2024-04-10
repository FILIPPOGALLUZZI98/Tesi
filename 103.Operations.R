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





gemp <- gem
# Mean value 1-year
gemp <- gemp %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mvalue1 = (lag(value) + value) / 2)














