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

########################################################################################################
########################################################################################################

# Shapefile
shp <- sf::read_sf("Data/Shapefile/shapefile.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Groundwater Storage
rs <- raster::brick("Data/GW/gwy.nc"); proj4string(rs) <- raster::crs(shp)
# Total Water Storage
rt <- raster::brick("Data/GW/twsy.nc"); proj4string(rt) <- raster::crs(shp)
# Runoff (??)
rq <- raster::brick("Data/GW/qry.nc"); proj4string(rq) <- raster::crs(shp)

########################################################################################################
########################################################################################################

##  shp -----------> file contenente le geometrie delle regioni
##  rs  -----------> file contenente i rasterbrick dell GW storage
##  rt  -----------> file contenente i rasterbrick del TWS
##  rq  -----------> file contenente i rasterbrick del runoff
##  
##  
##  
##  
##  
##  
## 
