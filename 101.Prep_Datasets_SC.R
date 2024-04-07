# This is the code to prepare the datasets for single countries from the modified shapefile and
# raster (see Preparazione/100.Data_Prep.R). This code will create a new repository with the name of the 
# country that contains the datasets.
# This is a list of the saved datasets:
# GW --> (year, region, value)
# Conflict --> (year, region, lat, lon, type, number_deaths)  this contains only the obseerved ones, there are no zeros
# GW-Conflict --> (year, region, type, number_deaths, value) the dataset contains also zeros in the number columns if in
                                                     # that year and region no conflict happened
# Migr --> () 
# GW-Migr -->()


# Select the country
paese <- "Nigeria"
# Select the raster
rast <- "gws"    ## gws; ...

##############################################################################################################################
##############################################################################################################################
suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

# Select shapefile and raster
shp <- st_read("^Data/shp/shp.shp")
r <- raster::brick(paste0("^Data/",rast,".nc"))

# Subset of the shapefile for the selected country
state <- subset(shp, country == paese)    ## plot(state[,"geom"])

# Create a directory for datasets
percorso_cartella <- paste0("^Data/Single_Country/",paese,"/")
if (!file.exists(percorso_cartella)) {
  dir.create(percorso_cartella, recursive = TRUE)
}

##############################################################################################################################
####  GW DATA  ###############################################################################################################

# Merging data gw-shapefile
gw_sc <- exactextractr::exact_extract(r, state, fun="mean")

# I add a column for the regions
gw_sc$region <- state$region

# I reshape the dataset into a long form
gw_sc <- reshape2::melt(gw_sc, id.vars="region")

# I rename the years
anni <- 1901:2019
gw_sc <- gw_sc %>%
  group_by(region) %>%
  mutate(year = anni)
gw_sc$variable=NULL
gw_sc <- gw_sc[, c("year", "region", "value")]

# Save data
write.csv(gw_sc, paste0(percorso_cartella, paese, "_",rast, ".csv"), row.names=FALSE)

#############################################################################################################################
####  POINT DATA CONFLICT UPPSALA  ##########################################################################################

# Select the conflict data
file_path <- paste("^Data_Raw/Conflict_Data/", paese, ".csv", sep = "")
events <- read.csv(file_path)

# Select the variables of interest
events <- events[, c("relid", "code_status","latitude" ,"longitude", "best_est")]

# Rename the variables
events <- events %>%
  rename(year = relid,
         type = code_status)
events <- mutate(events,
                 type = case_when(
                   type == 1 ~ "state",
                   type == 2 ~ "Nstate",
                   type == 3 ~ "onesided"
                 ))

# Set the coordinate system
events$latitude=NULL
events$latitude <- as.numeric(str_extract(events$longitude, "(?<=POINT \\()[0-9.-]+"))
events$longitude <- as.numeric(str_extract(events$longitude, "(?<=\\s)[0-9.-]+(?=\\))"))
events <- na.omit(events[, c("year", "type","latitude" ,"longitude","best_est")])
events = sf::st_as_sf(events, coords = c("latitude","longitude"), remove = FALSE)
sf::st_crs(events) <- sf::st_crs(state)
events$longitude <- st_coordinates(events)[, "X"]
events$latitude <- st_coordinates(events)[, "Y"]

# Intersection shapefile-events
intersection <- sf::st_intersects(events, state)            

# Set non-matched values to NA, these points are recorded in no province (outliers, miscoding, ...)
intersection[sapply(intersection, length) == 0] <- NA    

# Merge region name to conflict data points
events$region <- state$region[unlist(intersection)]   
events$geometry <- NULL

# Group events for the same region and type in one year 
events <- events %>% group_by(year, region, type, latitude, longitude) %>% summarise(number_deaths = n())

# Sort datasets by year
events <- events[order(events$year),]
events <- events[, c("year", "region", "latitude", "longitude", "type", "number_deaths")]

# Save data
write.csv(events,paste0(percorso_cartella, paese, "_events.csv"), row.names=FALSE)

#############################################################################################################################
####  MIGRATION DATASET  ####################################################################################################



##############################################################################################################################
####  JOINT DATASET GW-CONFLITC  ##############################################################################################

# Select for GW data the same timespan as for conflicts
# Create a dataset in which I have for each year and for each region the value of GW, the number of
# conflicts (that could be 0 if there were none) and the type of conflict

gw_data_sc <- gw_sc %>%
  filter(year > 1988)
gw_events_sc <- expand.grid(year = 1989:2019, region = unique(events$region),type=c("state","Nstate","onesided"))
gw_events_sc <- left_join(gw_events_sc, events, by=c("region", "year", "type"))
gw_events_sc$number_deaths[is.na(gw_events_sc$number_deaths)] = 0  ## Assign a zero to each month/province where no data is observed
gw_events_sc$latitude = NULL ; gw_events_sc$longitude=NULL
gw_events_sc <- left_join(gw_events_sc, gw_data_sc[,c("year", "region","value")], by=c("year", "region"))

gw_events_sc <- gw_events_sc[, c("year", "region", "type", "number_deaths", "value")]

# Save data
write.csv(gw_events_sc, paste0(percorso_cartella, paese, "_", rast, "_events", ".csv"), row.names=FALSE)

##############################################################################################################################
####  JOINT DATASET GW-MIGRATIONS  ###########################################################################################









