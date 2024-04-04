suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

############################################################################################################################
############################################################################################################################

shp <- st_read("^Data/Shapefile/shp.gpkg")
r <- raster::brick("^Data/Raster/gws.nc")

state <- subset(shp, CNTRY_NAME == "Australia")
plot(r$X1)
plot(state, add = TRUE)
