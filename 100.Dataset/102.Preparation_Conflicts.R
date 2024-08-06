# This code is used to prepare and reshape the raw datasets

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer); library(xtable)} )

#################################################################################################
#################################################################################################
######  MERGING THE CONFLICT EVENTS IN THE REGIONS OF THE SHAPEFILE

# Select the raw conflict data and the shapefile
events <- read.csv("^Data/^Raw_Data/Conflict_Data/Global.csv")
shp <- st_read("^Data/shp/shp.shp")

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
                   type == 3 ~ "onesided"))

# Set the coordinate system
events <- st_as_sf(events, coords = c("longitude", "latitude"), crs = st_crs(shp))
events <- st_transform(events, st_crs(shp))

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


#################################################################################################
#################################################################################################
######  JOINT DATASET GWS - EVENTS

# Open the datasets
gws <- read.csv("^Data/gws.csv")
events <- read.csv("^Data/events.csv")
pop <- read.csv("^Data/population.csv")

events_data <- events %>%
  filter(year<2020)

vettore <- expand.grid(year=1944:2019, type=c("state","Nstate","onesided"))
gws_events <- left_join(gws, vettore, by=c("year"))

# Merge the datasets
gws_events <- left_join(gws_events,events_data,by=c("country","region","year","type","orig"))
gws_events$deaths[is.na(gws_events$deaths)] = 0  ## Assign a zero to each month/province where no data is observed
gws_events$conflicts[is.na(gws_events$conflicts)] = 0  ## Assign a zero to each month/province where no data is observed
gws_events <- gws_events[, c("year","country", "region","type","deaths", "conflicts","value","orig")]

# Merge with population values
gws_events <- merge(gws_events, pop, by = c("year", "country", "region"), all.x = TRUE)

# Save data
write.csv(gws_events, paste0("^Data/", "gws_events_j", ".csv"), row.names=FALSE)


#################################################################################################
#################################################################################################
######  OPERATIONS FOR CONFLICT COORDINATES PLOT DATASET

# This code is used to create a dataset just for plotting the coordinates of the conflicts
# The same code is copied below (because the saving of this file was continuing to give me errors)

# Select the raw conflict data and the shapefile
events <- read.csv("^Data/^Raw_Data/Conflict_Data/Global.csv")
shp <- st_read("^Data/shp/shp.shp")

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
                   type == 3 ~ "onesided"))

# Set the coordinate system
events <- st_as_sf(events, coords = c("longitude", "latitude"), crs = st_crs(shp))
events <- st_transform(events, st_crs(shp))

# Save Dataset
st_write(events, "^Data/events_coordinates", driver = "ESRI Shapefile")
# This dataset contains the coordinate for the conflicts


