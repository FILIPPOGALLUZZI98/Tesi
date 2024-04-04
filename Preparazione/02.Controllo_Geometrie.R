suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

############################################################################################################################
############################################################################################################################

shp <- st_read("^Data/Shapefile/shp.gpkg")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
r <- raster::brick("^Data/Raster/gws.nc"); proj4string(r) <- raster::crs(shp)

state <- subset(shp, CNTRY_NAME == "Australia")
plot(r$X1)
plot(state, add = TRUE)
