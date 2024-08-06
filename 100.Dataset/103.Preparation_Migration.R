# This code is used to prepare and reshape the raw datasets

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer); library(xtable)} )


#################################################################################################
#################################################################################################
######  JOINT DATASET GWS - MIGR

# Open the datasets
gws <- read.csv("^Data/gws.csv")
pop <- read.csv("^Data/population.csv")
migr <-read.csv("^Data/^Raw_Data/Global_migr_raw.csv")

# Sort the order of the variables ofmigr dataset
migr <- data_migr[,c("year", "country_name", "worldregion", "population","mig_interval","year_cat10","flow","flow_annual", "outflow_rate_annual", "orig")]

# Rename variables
migr <- migr %>%
  rename(country = country_name, 
         interval=mig_interval)

# Convert the values of 'orig' in gws into integers
gws$orig <- as.integer(gws$orig)

# Merge the datasets
gws_migr <- left_join(gws, migr, by=c("year", "orig"))

# Sort and rename the variables
gws_migr <- gws_migr %>%
  rename(country=country.x)
gws_migr$country.y=NULL
gws_migr <- gws_migr[,c("year", "country", "region", "worldregion", "value", "population","interval", "flow","flow_annual",
                      "outflow_rate_annual","year_cat10", "orig")]

# Merge with population values
gws_migr <- merge(gws_migr, pop, by = c("year", "country", "region"), all.x = TRUE)

# Save data
write.csv(gws_migr, paste0("^Data/", "gws_migr_j", ".csv"), row.names=FALSE)



























