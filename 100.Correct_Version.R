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
####  CARICARE I DATI  #########################################################################
shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
elementi <- c(34, 381, 2070, 419, 420, 2071, 643, 770, 868, 930, 2072, 1065, 
              1105, 1162, 1542, 1548, 1578, 1824, 1968, 2073)
shp<- shp[-elementi,]
shp <- sf::st_transform(mshp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
r <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")
proj4string(r) <- raster::crs(shp)
#################################################################################################
####  CARICARE I DATI  #########################################################################
nomi <- unique(shp$CNTRY_NAME)
for (i in seq_along(nomi)) {
  a <- subset(shp, CNTRY_NAME == nomi[i])
  state <- append(state, list(a))
}

for (i in 1:264) {
    b <- exactextractr::exact_extract(r, state[[i]], fun="mean")
    b$region <- state[[i]]$ADMIN_NAME
    gw_data <- append(gw_data, list(b))
}

country <- "Italy"
x <- which(nomi == country)
ggplot(state[[x]], aes(fill=gw_data[[x]]$mean.X117)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)





















