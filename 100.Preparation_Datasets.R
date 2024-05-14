# This code is used to prepare and reshape the datasets used for the statistical analysis
#################################################################################################
# Packages

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})




#################################################################################################
#################################################################################################
######  INITIAL OPERATIONS FOR THE SHAPEFILE

# Open the shapefile and remove undesired variables
shp <- sf::read_sf("^Data_Raw/world_geolev1_2021/world_geolev1_2021.shp")
shp$BPL_CODE=NULL; shp$CNTRY_CODE=NULL

# Remove regions with geometry error
empty <- st_is_empty(shp); shp <- shp[!empty, ]
# Remove the invalid geometries from shp
shp <- shp[st_is_valid(shp), ]

# List of empty geometry regions
nomi_geometrie_vuote_rimosse <- rownames(shp)[empty]; print(nomi_geometrie_vuote_rimosse)

# Rename the variables country and region
shp <- shp %>%
  rename(country = CNTRY_NAME,
         region = ADMIN_NAME)
# Set the country name equal to the region if the country has no subregions
shp$region <- ifelse(is.na(shp$region), shp$country, shp$region)

# Set the CRS
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Save data
st_write(shp, "^Data/separate/shp", driver = "ESRI Shapefile")

#################################################################################################
#################################################################################################
######  INITIAL OPERATIONS FOR THE RASTER OF GROUNDWATER STORAGE 

# Open the raster that contains the groundwater storage and the shapefile
r <- raster::brick("^Data_Raw/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_groundwstor_global_monthly_1901_2019.nc")
shp <- st_read("^Data/separate/shp/shp.shp")

# Set the same CRS of the shapefile
proj4string(r) <- raster::crs(shp)

# Annual mean for all the rasters
media_annuale <- lapply(1:119, function(i) {
  anno_iniziale <- (i - 1) * 12 + 1
  anno_finale <- i * 12
  media <- mean(r[[anno_iniziale:anno_finale]])
  return(media)
})

# New rasterbrick with annual averaged values
gws <- brick(media_annuale)

# Save data
years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
names(gws) <- paste0("gws", years)
output_nc <- "^Data/separate/gws.nc"
writeRaster(gws, filename = output_nc, format = "CDF", overwrite = TRUE)


#################################################################################################
#################################################################################################
######  MERGING THE SHAPEFILE AND THE RASTER

# Open shapefile and raster
shp <- st_read("^Data/separate/shp/shp.shp")
r <- raster::brick(paste0("^Data/separate/gws",".nc"))

# Merging data
gw <- exactextractr::exact_extract(r, shp, fun="mean")

# Add columns for regions and countries
gw$region <- shp$region ; gw$country <- shp$country; gw$orig<-shp$GEOLEVEL1

# Reshape the dataset into a long form
gw <- reshape2::melt(gw, id.vars=c("country", "region", "orig"))

# Rename the years
gw$variable <- gsub("mean.X", "", gw$variable)  # Rimuovi "mean.X"
gw$year <- as.integer(gsub("\\D", "", gw$variable)) + 1900 
gw$variable=NULL
gw <- gw[, c("year","country", "region", "value","orig")]

# Save Dataset
write.csv(gw, paste0("^Data/separate/", "gws", ".csv"), row.names=FALSE)
# I obtained a dataset called 'gws' with the value of groundwater storage ine each region of the 
# shapefile and for each year


#################################################################################################
#################################################################################################
######  INITIAL OPERATIONS FOR CONFLICT DATASET

# Select the raw conflict data and the shapefile
events <- read.csv("^Data_Raw/Conflict_Data/Global.csv")
shp <- st_read("^Data/separate/shp/shp.shp")

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

# Save Dataset
write.csv(events, paste0("^Data/separate/", "events_coordinates", ".csv"), row.names=FALSE)
# This dataset contains the coordinate for the conflicts

#################################################################################################
#################################################################################################
######  MERGING SHAPEFILE AND CONFLICTS

events <- read.csv("^Data/separate/events.csv")
shp <- sf::read_sf("^Data/separate/shp.shp")

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
write.csv(events, paste0("^Data/separate/", "events", ".csv"), row.names=FALSE)


#################################################################################################
#################################################################################################
######  INITIAL OPERATIONS FOR MIGRATION DATASET

data_migr <-read.csv("^Data_Raw/Global_migr_raw.csv")
# Sort the order of the variables
data_migr <- data_migr[,c("year", "country_name", "worldregion", "population","mig_interval","year_cat10","flow","flow_annual", "outflow_rate_annual", "orig")]
# Rename some variables
data_migr <- data_migr %>%
  rename(country = country_name, 
         interval=mig_interval)

# Save data
write.csv(data_migr, paste0("^Data/separate/", "migr", ".csv"), row.names=FALSE)





















































