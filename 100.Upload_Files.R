shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
# Rimuovo le regioni che danno geometry error
elementi <- c(34, 381, 2070, 419, 420, 2071, 643, 770, 868, 930, 2072, 1065, 
              1105, 1162, 1542, 1548, 1578, 1824, 1968, 2073)
shp<- shp[-elementi,]
# Raster è quello modificato da me medie annuali e non mensili
rg <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")  ## groundwstrg
rt <- raster::brick("GW_Data/ISIMIP3a/twsy.nc")  ## total water storage
rq <- raster::brick("GW_Data/ISIMIP3a/qry.nc")  ## runoff (??)
proj4string(r) <- raster::crs(shp)

r <- rg  ## Nei codici seguenti il file raster sarà chiamato 'r', quindi 
         ## devo scegliere quale usare
