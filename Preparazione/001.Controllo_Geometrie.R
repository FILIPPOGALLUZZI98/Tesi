# This is the code to check if the geometries of the shapefile and the raster coincide

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

############################################################################################################################
############################################################################################################################

shp <- st_read("^Data/shp/shp.shp")
r <- raster::brick("^Data/gws.nc")

state <- subset(shp, country == "Australia")
plot(r$X1)
plot(state, add = TRUE)
