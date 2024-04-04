# Scegliere quale raster usare (rs, rt, rq)
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
r <- raster::brick(paste0("Data/GW/",rast,"y.nc")); proj4string(r) <- raster::crs(shp)

##############################################################################################
##############################################################################################


gw_data_g <- exactextractr::exact_extract(r, shp, fun="mean")
gw_data_g$region <- shp$ADMIN_NAME
gw_data_g$country <- shp$CNTRY_NAME
gw_data_g <- reshape2::melt(gw_data_g, id.vars=c("country", "region"))
gw_data_g$region <- ifelse(is.na(gw_data_g$region), gw_data_g$country, gw_data_g$region)
gw_data_g$variable <- gsub("mean.X", "", gw_data_g$variable)  # Rimuovi "mean.X"
gw_data_g$variable <- as.integer(gsub("\\D", "", gw_data_g$variable)) + 1900 

# gw_data_g <- left_join(shp, gw_data_g, by=c("ADMIN_NAME"="region")) 

##############################################################################################
##############################################################################################


file_path <- "Data/Conflict/Global.csv"
events <- read.csv(file_path)


## DA FARE


##############################################################################################
##############################################################################################
















