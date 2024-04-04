shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Groundwater Storage
rs <- raster::brick("Data/GW.gwy.nc"); proj4string(rs) <- raster::crs(shp)
# Total Water Storage
rt <- raster::brick("Data/GW.twsy.nc"); proj4string(rt) <- raster::crs(shp)
# Runoff (??)
rq <- raster::brick("Data/GW.qry.nc"); proj4string(rq) <- raster::crs(shp)

plot(rs$X1)
plot(shp[,"geometry"], add=TRUE) 
plot(state, add = TRUE)












