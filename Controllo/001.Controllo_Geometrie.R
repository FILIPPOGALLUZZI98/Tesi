# This is the code to check if the geometries of the modified shapefile and the raster coincide
# See "Preparazione/100.Data_Prep.R"

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
