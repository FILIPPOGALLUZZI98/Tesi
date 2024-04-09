# This is the code to prepare the datasets for global data from the modified shapefile and
# raster (see 100.Prep_Shapefile_Raster.R). This code will create the datasets of global variables.
# This is a list of the saved datasets:
# GW --> (year, country, region, value)
# Conflict --> (year, country, region, type, conflicts)  this contains only the observed ones, there are no zeros
# Deaths --> (year, country, region, type, number_deaths)  this contains only the observed ones, there are no zeros
# GW-Conflict --> (year, country, region, type, conflicts, value) the dataset contains also zeros in the number columns if in
                                                               # that year and region no conflict happened
# GW-Deaths --> (year, country, region, type, number_deaths, value) the dataset contains also zeros in the number columns if in
                                                               # that year and region no conflict happened
# Migr --> () 
# GW-Migr -->()


# Select the raster (gws, ...)
rast <- "gws"

##############################################################################################################################
##############################################################################################################################

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

# Select shapefile and raster
shp <- st_read("^Data/shp/shp.shp")
r <- raster::brick(paste0("^Data/",rast,".nc"))


##############################################################################################
####  GLOBAL GW  #############################################################################

# Merging data gw-shapefile
gw_g <- exactextractr::exact_extract(r, shp, fun="mean")

# Add columns for regions and countries
gw_g$region <- shp$region ; gw_g$country <- shp$country; gw_g$CNTRY_CODE<-shp$CNTRY_CODE
gw_g$BPL_CODE <- shp$BPL_CODE; gw_g$GEOLEVEL1<-shp$GEOLEVEL1

# Reshape the dataset into a long form
gw_g <- reshape2::melt(gw_g, id.vars=c("country", "region", "CNTRY_CODE","BPL_CODE","GEOLEVEL1"))

# Rename the years
gw_g$variable <- gsub("mean.X", "", gw_g$variable)  # Rimuovi "mean.X"
gw_g$year <- as.integer(gsub("\\D", "", gw_g$variable)) + 1900 
gw_g$variable=NULL
gw_g <- gw_g[, c("year","country", "region", "value","CNTRY_CODE", "BPL_CODE","GEOLEVEL1")]

# Save Data
write.csv(gw_g, paste0("^Data/", "gws", ".csv"), row.names=FALSE)

##############################################################################################
####  GLOBAL CONFLICT UPPSALA (N_DEATHS+CONFLICTS)  ##########################################

# Select the conflict data
file_path <- "^Data_Raw/Conflict_Data/Global.csv"
events <- read.csv(file_path)

# Select the variables of interest
events <- events[, c("country" ,"year", "type_of_violence","latitude" ,"longitude", "best")]

# Rename the variables
events <- events %>%
  rename(type = type_of_violence,
         number_deaths = best)
events <- mutate(events,
                 type = case_when(
                   type == 1 ~ "state",
                   type == 2 ~ "Nstate",
                   type == 3 ~ "onesided"
                 ))

# Set the coordinate system
events <- st_as_sf(events, coords = c("longitude", "latitude"), crs = st_crs(shp))
events <- st_transform(events, st_crs(shp))

# Remove the invalid geometries from shp
shp <- shp[st_is_valid(shp), ]

# Intersection shapefile-events and aggregate data 
events_joined <- st_join(events, shp)
events_joined <- events_joined %>%
  rename(country = country.y)
events_joined$geometry=NULL
events_joined$country.x=NULL



events1 <- events_joined %>%
  group_by(year, country, region, type, CNTRY_CODE, BPL_CODE, GEOLEVEL1) %>%
  summarise(deaths = sum(number_deaths, na.rm = TRUE))

events2 <- events_joined %>%
  group_by(year, country, region, type, CNTRY_CODE, BPL_CODE, GEOLEVEL1) %>%
  summarise(conflicts = n())
events <- left_join(events1, events2, by=c("year", "country","region","type", "CNTRY_CODE", "BPL_CODE", "GEOLEVEL1"))
events <- events[, c("year","country", "region","type","deaths", "conflicts","CNTRY_CODE", "BPL_CODE", "GEOLEVEL1")]

# Sort datasets by year
events <- events[order(events$country),]
events <- events[order(events$year),]

# Save Data
write.csv(events, paste0("^Data/", "events", ".csv"), row.names=FALSE)


##############################################################################################################################
####  GLOBAL JOINT DATASET GW-EVENTS  ########################################################################################

gw_data_g <- gw_g %>%
  filter(year > 1988)
events <- events %>%
  filter(year<2020)
vettore <- expand.grid(year=1989:2019, type=c("state","Nstate","onesided"))
gw_events_g <- left_join(gw_data_g, vettore, by=c("year"))

# Merge the datasets
gw_events <- left_join(gw_events_g,events,by=c("country","region","year","type", "CNTRY_CODE", "BPL_CODE", "GEOLEVEL1"))
gw_events$deaths[is.na(gw_events$deaths)] = 0  ## Assign a zero to each month/province where no data is observed
gw_events$conflicts[is.na(gw_events$conflicts)] = 0  ## Assign a zero to each month/province where no data is observed
gw_events <- gw_events[, c("year","country", "region","type","deaths", "conflicts","value", "CNTRY_CODE", "BPL_CODE", "GEOLEVEL1")]

# Save data
write.csv(gw_events, paste0("^Data/", "gws_events", ".csv"), row.names=FALSE)


#############################################################################################################################
####  GLOBAL MIGRATION DATASET  #############################################################################################

data_migr <-read.csv("^Data_Raw/Global_migr_raw.csv")
# Sort the order of the variables
data_migr <- data_migr[,c("year", "country_name", "worldregion", "population","mig_interval","year_cat10","flow","flow_annual", "outflow_rate_annual", "orig")]
# Rename some variables
data_migr <- data_migr %>%
  rename(country = country_name, 
         interval=mig_interval)

# Save data
write.csv(data_migr, paste0("^Data/", "migr", ".csv"), row.names=FALSE)


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




