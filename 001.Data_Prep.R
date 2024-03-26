#################################################################################################
####  SHAPEFILE  ############################################################################
shp <- sf::read_sf("Data_Raw/world_geolev1_2021/world_geolev1_2021.shp")
# Rimuovo le regioni che danno geometry error
elementi <- c(34, 381, 2070, 419, 420, 2071, 643, 770, 868, 930, 2072, 1065, 
              1105, 1162, 1542, 1548, 1578, 1824, 1968, 2073)
shp<- shp[-elementi,]
# Rimuovo le variabili che non mi servono
shp$BPL_CODE <- NULL; shp$CNTRY_CODE <- NULL; shp$GEOLEVEL1 <- NULL
st_write(obj = shp, dsn = "Data", layer = "shapefile", driver = "ESRI Shapefile")




#################################################################################################
####  GROUNDWATER STORAGE YEAR AVERAGE  #########################################################
r <- raster::brick("Data_Raw/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_groundwstor_global_monthly_1901_2019.nc")

# Creazione di un nuovo raster brick per le medie annuali
gwy <- raster::brick(ncol = ncol(r), nrow = nrow(r), nl = nlayers(r)/12,
                     xmn = extent(r)[1], xmx = extent(r)[2], 
                     ymn = extent(r)[3], ymx = extent(r)[4], 
                     crs = crs(r))

# Ciclo for per calcolare le medie annuali
for (i in 1:(nlayers(r)/12)) {
  start <- (i - 1) * 12 + 1
  end <- start + 11
  yearly_mean <- calc(r[[start:end]], mean)
  gwy[[i]] <- yearly_mean
}

# Salvataggio dati
years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
names(gwy) <- paste0("gw", years)
output_nc <- "Data/gwy.nc"
# Scrivi il RasterBrick in un file NetCDF
writeRaster(gwy, filename = output_nc, format = "CDF", overwrite = TRUE)




#############################################################################################################################
####  TOTAL WATER STORAGE YEAR AVERAGE  #####################################################################################
r <- raster::brick("Data_Raw/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_tws_global_monthly_1901_2019.nc")

# Creazione di un nuovo raster brick per le medie annuali
twsy <- raster::brick(ncol = ncol(r), nrow = nrow(r), nl = nlayers(r)/12,
                      xmn = extent(r)[1], xmx = extent(r)[2], 
                      ymn = extent(r)[3], ymx = extent(r)[4], 
                      crs = crs(r))

# Ciclo for per calcolare le medie annuali
for (i in 1:(nlayers(r)/12)) {
  start <- (i - 1) * 12 + 1
  end <- start + 11
  yearly_mean <- calc(r[[start:end]], mean)
  twsy[[i]] <- yearly_mean
}

# Salvataggio dati
years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
names(twsy) <- paste0("tws", years)
output_nc <- "Data/twsy.nc"
# Scrivi il RasterBrick in un file NetCDF
writeRaster(twsy, filename = output_nc, format = "CDF", overwrite = TRUE)




#############################################################################################################################
####  RUNOFF YEAR AVERAGE  ##################################################################################################
r <- raster::brick("Data_Raw/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_qr_global_monthly_1901_2019.nc")

# Creazione di un nuovo raster brick per le medie annuali
qry <- raster::brick(ncol = ncol(r), nrow = nrow(r), nl = nlayers(r)/12,
                     xmn = extent(r)[1], xmx = extent(r)[2], 
                     ymn = extent(r)[3], ymx = extent(r)[4], 
                     crs = crs(r))

# Ciclo for per calcolare le medie annuali
for (i in 1:(nlayers(r)/12)) {
  start <- (i - 1) * 12 + 1
  end <- start + 11
  yearly_mean <- calc(r[[start:end]], mean)
  qry[[i]] <- yearly_mean
}

# Salvataggio dati
years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
names(qry) <- paste0("qr", years)
output_nc <- "Data/qry.nc"
# Scrivi il RasterBrick in un file NetCDF
writeRaster(qry, filename = output_nc, format = "CDF", overwrite = TRUE)




#############################################################################################################################
####    ###############################################################################################################




































