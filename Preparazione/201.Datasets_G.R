# Select the raster (gws, ...)
rast <- "gws"

##############################################################################################################################
##############################################################################################################################
suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

# Select shapefile and raster
shp <- st_read("^Data/shp.gpkg")
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
####  POINT DATA CONFLICT UPPSALA  ###########################################################

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
events <- na.omit(events[, c("country" ,"year", "type","latitude" ,"longitude","number")])
events <- sf::st_as_sf(events, coords = c("latitude","longitude"), remove = FALSE)
sf::st_crs(events) <- sf::st_crs(shp)
events$longitude <- st_coordinates(events)[, "X"]
events$latitude <- st_coordinates(events)[, "Y"]
intersection <- sf::st_intersects(events, shp)            
intersection[sapply(intersection, length) == 0] <- NA    









































