rs <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")  ## groundwstrg
rt <- raster::brick("GW_Data/ISIMIP3a/twsy.nc")  ## total water storage
rq <- raster::brick("GW_Data/ISIMIP3a/qry.nc")  ## runoff (??)
proj4string(rs) <- raster::crs(shp)
proj4string(rt) <- raster::crs(shp)
proj4string(rq) <- raster::crs(shp)
