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
gw_g$region <- shp$region ; gw_g$country <- shp$country

# Reshape the dataset into a long form
gw_g <- reshape2::melt(gw_g, id.vars=c("country", "region"))

# Rename the years
gw_g$variable <- gsub("mean.X", "", gw_g$variable)  # Rimuovi "mean.X"
gw_g$year <- as.integer(gsub("\\D", "", gw_g$variable)) + 1900 
gw_g$variable=NULL

# Save data
write.csv(gw_g, paste0("^Data/", "Global_",rast, ".csv"), row.names=FALSE)

##############################################################################################
####  GLOBAL CONFLICT UPPSALA  ###############################################################

# Select the conflict data
file_path <- "^Data_Raw/Conflict_Data/Global.csv"
events <- read.csv(file_path)

# Select the variables of interest
events <- events[, c("country" ,"year", "type_of_violence","latitude" ,"longitude", "best")]

# Rename the variables
events <- events %>%
  rename(type = type_of_violence,
         number = best)
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
events_joined$country.x=NULL
events_joined <- events_joined %>%
  rename(country = country.y)
events <- aggregate(number ~ year + country + region + type, data = events_joined, sum)

# Sort datasets by year
events <- events[order(events$country),]
events <- events[order(events$year),]

# Save data
write.csv(events, paste0("^Data/", "Global_events", ".csv"), row.names=FALSE)

#############################################################################################################################
####  GLOBAL MIGRATION DATASET  #############################################################################################



##############################################################################################################################
####  GLOBAL JOINT DATASET GW-CONFLITC  ######################################################################################

gw_data_g <- gw_g %>%
  filter(year > 1989)
gw_events_g <- expand.grid(year = 1990:2022, region = unique(events$region),type=c("state","Nstate","onesided"))
gw_events_g <- left_join(gw_events_g, events, by=c("region", "year", "type"="type"))
gw_events_g$number[is.na(gw_events_g$number)] = 0  ## Assign a zero to each month/province where no data is observed
gw_events_g$latitude = NULL ; gw_events_g$longitude=NULL
gw_events_g <- gw_events_g %>%
  filter(year != 2020 & year != 2021 & year != 2022)
gw_events_g <- left_join(gw_events_g, gw_data_g[,c("year", "region","value")], by=c("year", "region"))

gw_events_g$country <- ifelse(is.na(gw_events_g$country), gw_events_g$region, gw_events_g$country)

# Save data
write.csv(gw_events_g, paste0("^Data/", "Global_events_gws", ".csv"), row.names=FALSE)













