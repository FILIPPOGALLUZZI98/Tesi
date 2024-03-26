#################################################################################################
####  SHAPEFILE  ############################################################################
shp_orig <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp_orig, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
# Rimuovo le regioni che danno geometry error
elementi <- c(34, 381, 2070, 419, 420, 2071, 643, 770, 868, 930, 2072, 1065, 
              1105, 1162, 1542, 1548, 1578, 1824, 1968, 2073)
shp<- shp[-elementi,]

# Rimuovo le variabili che non mi servono
shp$BPL_CODE <- NULL; shp$CNTRY_CODE <- NULL; shp$GEOLEVEL1 <- NULL




#################################################################################################
####  GROUNDWATER STORAGE  #######################################################################
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
####  TOTAL WATER STORAGE  ##################################################################################################
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
output_nc <- "GW_Data/ISIMIP3a/qry.nc"
# Scrivi il RasterBrick in un file NetCDF
writeRaster(qry, filename = output_nc, format = "CDF", overwrite = TRUE)




#############################################################################################################################
####  GW_DATA_LIST  ###############################################################################################################

gw_data_list <- list()
states <- list()
nomi <- unique(shp$CNTRY_NAME)

for (i in seq_along(nomi)) {
  a <- subset(shp, CNTRY_NAME == nomi[i])
  states <- append(states, list(a))
}

for (i in 1:283) {
  b <- exactextractr::exact_extract(r, states[[i]], fun="mean")
  b$region <- states[[i]]$ADMIN_NAME
  gw_data_list <- append(gw_data_list, list(b))
}

















