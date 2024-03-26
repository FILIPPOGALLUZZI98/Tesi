rs <- raster::brick("")  ## groundwstrg
rt <- raster::brick("")  ## total water storage
rq <- raster::brick("")  ## runoff (??)
proj4string(rs) <- raster::crs(shp)
proj4string(rt) <- raster::crs(shp)
proj4string(rq) <- raster::crs(shp)
