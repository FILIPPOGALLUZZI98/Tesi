# This is the code to prepare the shapefile and the raster
# Shapefile: remove the non useful variables; remove geometry errors; rename the variables; set the CRS
# Raster: set the CRS, annual mean

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

gw_g <- read.csv("^Data/gws.csv")
events <- read.csv("^Data/events.csv")
migr <- read.csv("^Data/migr.csv")

##############################################################################################################################
####  GLOBAL JOINT DATASET GW-EVENTS  ########################################################################################

gw_data_g <- gw_g %>%
  filter(year > 1988)
events <- events %>%
  filter(year<2020)
vettore <- expand.grid(year=1989:2019, type=c("state","Nstate","onesided"))
gw_events_g <- left_join(gw_data_g, vettore, by=c("year"))

# Merge the datasets
gw_events <- left_join(gw_events_g,events,by=c("country","region","year","type","GEOLEVEL1"))
gw_events$deaths[is.na(gw_events$deaths)] = 0  ## Assign a zero to each month/province where no data is observed
gw_events$conflicts[is.na(gw_events$conflicts)] = 0  ## Assign a zero to each month/province where no data is observed
gw_events <- gw_events[, c("year","country", "region","type","deaths", "conflicts","value","GEOLEVEL1")]

# Save data
write.csv(gw_events, paste0("^Data/", "gws_events", ".csv"), row.names=FALSE)


##############################################################################################################################
####  GLOBAL JOINT DATASET GW-MIGR  ##########################################################################################

# Select the timespan for GW
gw_data_g <- gw_g %>%
  filter(year > 1959 & year<2018)
# Rename the variable
gw_data_g <- gw_data_g %>%
  rename(orig=GEOLEVEL1)

# Check for countries without GEOLEV1
na_val <- subset(gw_data_g, is.na(orig))$country
a <- unique(countries_with_na); b <- unique(data_migr$country)
intersect(a, b)
# Check if the datasets gw_data_g cointains all the regions of data_migr
length(intersect(unique(gw_data_g$orig), data_migr$orig))
length(qunique(data_migr$orig))

# Remove NA values from gw_data_g
# gw_data_g <- na.omit(gw_data_g[!is.na(gw_data_g$orig), ])

# Convert the values of 'orig' in gw_data_g into integers
gw_data_g$orig <- as.integer(gw_data_g$orig)

# Merge the datasets
gw_migr <- left_join(gw_data_g, data_migr, by=c("year", "orig"))

# Remove NA values
# gw_migr <- na.omit(gw_migr[!is.na(gw_migr$population), ])

# Sort and rename the variables
gw_migr <- gw_migr %>%
  rename(country=country.x)
gw_migr$country.y=NULL
gw_migr <- gw_migr[,c("year", "country", "region", "worldregion", "value", "population","interval", "flow","flow_annual",
                      "outflow_rate_annual","year_cat10", "orig", "CNTRY_CODE","BPL_CODE")]

# Save data
write.csv(gw_migr, paste0("^Data/", "gws_migr", ".csv"), row.names=FALSE)




