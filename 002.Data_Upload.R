

rs <- raster::brick("Data/GW.gwy.nc")  ## groundwstrg
rt <- raster::brick("Data/GW.twsy.nc")  ## total water storage
rq <- raster::brick("Data/GW.qry.nc")  ## runoff (??)
proj4string(rs) <- raster::crs(shp)
proj4string(rt) <- raster::crs(shp)
proj4string(rq) <- raster::crs(shp)


