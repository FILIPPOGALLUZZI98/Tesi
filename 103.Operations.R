suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr)} )

ge <- read.csv("^Data/gws_events.csv")
gem <- read.csv("^Data/gws_migr_events.csv")

# gem and ge have different rownumber because ge cointains also the type (three values)

#################################################################################################
#################################################################################################

# Per ogni 'orig', per ogni 'type, per ogni 'year': media di 'value' con l'anno precedente e mettere in 'mvalue1'
# Per ogni 'orig', per ogni 'type, per ogni 'year': media di 'value' con i 5 anni precedenti e mettere in 'mvalue5'
# Per ogni 'orig', per ogni 'type, per ogni 'year': media di 'conflicts' con l'anno precedente e mettere in 'mconfl1'
# Per ogni 'orig', per ogni 'type, per ogni 'year': media di 'conflicts' con i 5 anni precedenti e mettere in 'mconfl5'



gem <- gem %>%
  arrange(type, orig, year) %>%
  group_by(orig, type) %>%
  mutate(mvalue1 = (value + lag(value, default = 0)) / 2)

gem <- gem %>%
  arrange(type, orig, year) %>%
  group_by(orig, type) %>%
  mutate(mvalue1 = (value + lag(value, default = 0)) / 2)

gem <- gem %>%
  arrange(type, orig, year) %>%
  group_by(orig, type) %>%
  mutate(mvalue1 = rollmean(value, k = 5, fill = NA, align = "right", na.pad = TRUE))










