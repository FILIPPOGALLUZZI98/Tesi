# Medie annuali dei dati raster ISIMIP3a del groundwstr a partire dai dati mensili

#############################################################################################################################
####  GROUNDWATER STORAGE  ###################################################################################################
r <- raster::brick("GW_Data/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_groundwstor_global_monthly_1901_2019.nc")

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
output_nc <- "GW_Data/ISIMIP3a/gwy.nc"
# Scrivi il RasterBrick in un file NetCDF
writeRaster(gwy, filename = output_nc, format = "CDF", overwrite = TRUE)


#############################################################################################################################
####  TWS  ##################################################################################################################
r <- raster::brick("GW_Data/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_tws_global_monthly_1901_2019.nc")

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
output_nc <- "GW_Data/ISIMIP3a/twsy.nc"
# Scrivi il RasterBrick in un file NetCDF
writeRaster(twsy, filename = output_nc, format = "CDF", overwrite = TRUE)


#############################################################################################################################
####  RUNOFF  ###############################################################################################################
r <- raster::brick("GW_Data/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_qr_global_monthly_1901_2019.nc")

# Creazione di un nuovo raster brick per le medie annuali
qr <- raster::brick(ncol = ncol(r), nrow = nrow(r), nl = nlayers(r)/12,
xmn = extent(r)[1], xmx = extent(r)[2], 
ymn = extent(r)[3], ymx = extent(r)[4], 
crs = crs(r))

# Ciclo for per calcolare le medie annuali
for (i in 1:(nlayers(r)/12)) {
  start <- (i - 1) * 12 + 1
  end <- start + 11
  yearly_mean <- calc(r[[start:end]], mean)
  qr[[i]] <- yearly_mean
}

# Salvataggio dati
years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
names(qr) <- paste0("qr", years)
output_nc <- "GW_Data/ISIMIP3a/qry.nc"
# Scrivi il RasterBrick in un file NetCDF
writeRaster(qr, filename = output_nc, format = "CDF", overwrite = TRUE)
