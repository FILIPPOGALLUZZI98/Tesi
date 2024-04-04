shp <- st_read("^Data/Shapefile/shp.gpkg")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
r <- raster::brick("^Data/Raster/gws.nc"); proj4string(r) <- raster::crs(shp)

state <- subset(shp, CNTRY_NAME == "France")
plot(r$X1)
plot(state, add = TRUE)













