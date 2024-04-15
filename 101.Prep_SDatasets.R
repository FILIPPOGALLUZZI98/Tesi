# This is the code to prepare the datasets for global data from the modified shapefile and
# raster (see 100.Prep_Shapefile_Raster.R). This code will create the datasets of global variables.

##############################################################################################################################
##############################################################################################################################

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

# Select the raster (gws, ...)
rast <- "gws"
# Select shapefile and raster
shp <- st_read("^Data/shp/shp.shp")
r <- raster::brick(paste0("^Data/",rast,".nc"))


##############################################################################################
####  GLOBAL GW  #############################################################################

# Merging data gw-shapefile
gw_g <- exactextractr::exact_extract(r, shp, fun="mean")

# Add columns for regions and countries
gw_g$region <- shp$region ; gw_g$country <- shp$country; gw_g$orig<-shp$GEOLEVEL1

# Reshape the dataset into a long form
gw_g <- reshape2::melt(gw_g, id.vars=c("country", "region", "orig"))

# Rename the years
gw_g$variable <- gsub("mean.X", "", gw_g$variable)  # Rimuovi "mean.X"
gw_g$year <- as.integer(gsub("\\D", "", gw_g$variable)) + 1900 
gw_g$variable=NULL
gw_g <- gw_g[, c("year","country", "region", "value","orig")]



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

# Create 2 variables: number of conflicts and deaths (per year)
events1 <- events_joined %>%
  group_by(year, country, region, type, GEOLEVEL1) %>%
  summarise(deaths = sum(number_deaths, na.rm = TRUE))

events2 <- events_joined %>%
  group_by(year, country, region, type, GEOLEVEL1) %>%
  summarise(conflicts = n())
events <- left_join(events1, events2, by=c("year", "country","region","type","GEOLEVEL1"))
events <- events[, c("year","country", "region","type","deaths", "conflicts","GEOLEVEL1")]

# Rename GEOLEVEL1 -> orig
events <- events %>%
  rename(orig = GEOLEVEL1)

# Sort datasets by year
events <- events[order(events$country),]
events <- events[order(events$year),]

# Save Data
write.csv(events, paste0("^Data/", "events", ".csv"), row.names=FALSE)


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





