suppressPackageStartupMessages({
  library(sf)              ## useful for spatial manipulations
  library(sp)              ## useful for spatial manipulations
  library(raster)          ## useful for working with raster data
  library(ncdf4)           ## useful for working with raster data
  library(exactextractr)   ## useful for extracting data from raster files
  library(dplyr)           ## useful for merging data sets
  library(reshape2)        ## useful for manipulating data sets
  library(ggplot2)         ## useful for data visualization
  library(ggrepel)         ## useful for labeling point plots in ggplot2
  library(lubridate)
  library(zoo)   
})


#################################################################################################
####  CARICARE E SISTEMARE I DATI  ##############################################################
# Upload dei dati shapefile + settaggio coordinate
shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 

# Upload dati delle medie annuali (non diretti ISIMIP3a) + settaggio coordinate
r <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")
proj4string(r) <- raster::crs(shp)

# Togliere le singole regioni dei paesi che danno errore (non tutti i paesi)
# Seleziono gli elementi di shp con geometry = EMPTY
elementi <- c(34, 381, 2070, 419, 420, 2071, 643, 770, 868, 930, 2072, 1065, 
              1105, 1162, 1542, 1548, 1578, 1824, 1968, 2073)
# Rimuovo questi elementi da shp
shp<- shp[-elementi,]


#################################################################################################
####  PLOT PER GLI ELEMENTI CHE DAVANO ERRORE  ###################################################
































