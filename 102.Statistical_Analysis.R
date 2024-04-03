# Select the country
country <- "Mexico"
# Select the year
y <- "2005"
# Select the raster
rast <- "rs"

suppressPackageStartupMessages({
  library(sf)              ## useful for spatial manipulations
  library(sp)              ## useful for spatial manipulations
  library(plyr )
  library(raster)          ## useful for working with raster data
  library(ncdf4)           ## useful for working with raster data
  library(exactextractr)   ## useful for extracting data from raster files
  library(dplyr)           ## useful for merging data sets
  library(stringr)
  library(reshape2)        ## useful for manipulating data sets
  library(ggplot2)         ## useful for data visualization
  library(ggrepel)         ## useful for labeling point plots in ggplot2
  library(lubridate)
  library(zoo)   
  library(foreign)
})

shp <- sf::read_sf("Data/Shapefile/shapefile.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
state <- subset(shp, CNTRY_NAME == country)    ## plot(state[,"geometry"])
state <- rename(state, region = ADMIN_NAME)
r <- raster::brick(paste0("Data/GW/",rast,"y.nc")); proj4string(r) <- raster::crs(shp)
path <- paste0("Data/GW_Conflict/", country, "/")
data_gw_events <- read.csv(paste0(path, country,"_gw_events_", rast,".csv"))
data_gw <- read.csv(paste0(path, country,"_gw_", rast,".csv"))
events <-read.csv(paste0(path, country, "_events.csv"))

######################################################################################################


data <- subset(data_gw_events, type=="Nstate")
data

lm <- lm(data$number ~ data$value + as.factor(data$year) + as.factor(data$region))
summary(lm)
plot(lm)


























