shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
# Rimuovo le regioni che danno geometry error
elementi <- c(34, 381, 2070, 419, 420, 2071, 643, 770, 868, 930, 2072, 1065, 
              1105, 1162, 1542, 1548, 1578, 1824, 1968, 2073)
shp<- shp[-elementi,]
# Raster è quello modificato da me medie annuali e non mensili
r <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")  ## groundwstrg
## r <- raster::brick("GW_Data/ISIMIP3a/twsy.nc")  ## total water storage
## r <- raster::brick("GW_Data/ISIMIP3a/qry.nc")  ## runoff (??)
proj4string(r) <- raster::crs(shp)



plot(shp[,"geometry"]) 
ext <- extent(-130, -60, 20, 50)  ## xmin, xmax, ymin, ymax
rc <- crop(r, ext)
state <- subset(shp, CNTRY_NAME == "United States")
plot(rc$X1)
plot(state, add = TRUE)












