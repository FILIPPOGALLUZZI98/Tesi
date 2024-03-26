# Shapefile
shp <- sf::read_sf("Data/Shapefile/shapefile.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Groundwater Storage
rs <- raster::brick("Data/GW/gwy.nc"); proj4string(rs) <- raster::crs(shp)
# Total Water Storage
rt <- raster::brick("Data/GW/twsy.nc"); proj4string(rt) <- raster::crs(shp)
# Runoff (??)
rq <- raster::brick("Data/GW/qry.nc"); proj4string(rq) <- raster::crs(shp)

########################################################################################################
########################################################################################################

##  shp -----------> file contenente le geometrie delle regioni
##  rs  -----------> file contenente i rasterbrick dell GW storage
##  rt  -----------> file contenente i rasterbrick del TWS
##  rq  -----------> file contenente i rasterbrick del runoff
##  
##  
##  
##  
##  
##  
##  


