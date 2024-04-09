# This is the code to prepare the shapefile and the raster
# Shapefile: remove the non useful variables; remove geometry errors; rename the variables; set the CRS
# Raster: set the CRS, annual mean

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

gw <- read.csv("^Data/gws.csv")
events <- read.csv("^Data/events.csv")
migr <- read.csv("^Data/migr.csv")

##############################################################################################################################
####  GLOBAL JOINT DATASET GW-EVENTS  ########################################################################################

gw_data <- gw %>%
  filter(year > 1988)
events_data <- events %>%
  filter(year<2020)

vettore <- expand.grid(year=1989:2019, type=c("state","Nstate","onesided"))
gw_events <- left_join(gw_data, vettore, by=c("year"))

# Merge the datasets
gw_events <- left_join(gw_events,events_data,by=c("country","region","year","type","orig"))
gw_events$deaths[is.na(gw_events$deaths)] = 0  ## Assign a zero to each month/province where no data is observed
gw_events$conflicts[is.na(gw_events$conflicts)] = 0  ## Assign a zero to each month/province where no data is observed
gw_events <- gw_events[, c("year","country", "region","type","deaths", "conflicts","value","orig")]


# Save data
write.csv(gw_events, paste0("^Data/", "gws_events", ".csv"), row.names=FALSE)


##############################################################################################################################
####  GLOBAL JOINT DATASET GW-MIGR  ##########################################################################################

# Select the timespan for GW
gw_data <- gw %>%
  filter(year > 1959 & year<2018)

# Convert the values of 'orig' in gw_data into integers
gw_data$orig <- as.integer(gw_data$orig)

# Merge the datasets
gw_migr <- left_join(gw_data, migr, by=c("year", "orig"))

# Sort and rename the variables
gw_migr <- gw_migr %>%
  rename(country=country.x)
gw_migr$country.y=NULL
gw_migr <- gw_migr[,c("year", "country", "region", "worldregion", "value", "population","interval", "flow","flow_annual",
                      "outflow_rate_annual","year_cat10", "orig")]

# Save data
write.csv(gw_migr, paste0("^Data/", "gws_migr", ".csv"), row.names=FALSE)



##############################################################################################################################
####  GLOBAL JOINT DATASET GW-MIGR-CONFL  ####################################################################################


gw_migr_conf <- left_join(gw_events, migr, by=c("year", "orig"))
gw_migr_conf <- gw_migr_conf %>%
  rename(country=country.x)
gw_migr_conf$country.y=NULL

# Save data
write.csv(gw_migr, paste0("^Data/", "gws_migr_events", ".csv"), row.names=FALSE)



















