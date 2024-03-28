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
rsy <- raster::brick(ncol = ncol(r), nrow = nrow(r), nl = nlayers(r)/12,
                     xmn = extent(r)[1], xmx = extent(r)[2], 
                     ymn = extent(r)[3], ymx = extent(r)[4], 
                     crs = crs(r))

# Ciclo for per calcolare le medie annuali
for (i in 1:(nlayers(r)/12)) {
  start <- (i - 1) * 12 + 1
  end <- start + 11
  yearly_mean <- calc(r[[start:end]], mean)
  rsy[[i]] <- yearly_mean
}

# Salvataggio dati
years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
names(rsy) <- paste0("gw", years)
output_nc <- "Data/rsy.nc"
# Scrivi il RasterBrick in un file NetCDF
writeRaster(rsy, filename = output_nc, format = "CDF", overwrite = TRUE)




#############################################################################################################################
####  TOTAL WATER STORAGE YEAR AVERAGE  #####################################################################################
r <- raster::brick("Data_Raw/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_tws_global_monthly_1901_2019.nc")

# Creazione di un nuovo raster brick per le medie annuali
rty <- raster::brick(ncol = ncol(r), nrow = nrow(r), nl = nlayers(r)/12,
                      xmn = extent(r)[1], xmx = extent(r)[2], 
                      ymn = extent(r)[3], ymx = extent(r)[4], 
                      crs = crs(r))

# Ciclo for per calcolare le medie annuali
for (i in 1:(nlayers(r)/12)) {
  start <- (i - 1) * 12 + 1
  end <- start + 11
  yearly_mean <- calc(r[[start:end]], mean)
  rty[[i]] <- yearly_mean
}

# Salvataggio dati
years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
names(rty) <- paste0("tws", years)
output_nc <- "Data/rty.nc"
# Scrivi il RasterBrick in un file NetCDF
writeRaster(rty, filename = output_nc, format = "CDF", overwrite = TRUE)




#############################################################################################################################
####  RUNOFF YEAR AVERAGE  ##################################################################################################
r <- raster::brick("Data_Raw/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_qr_global_monthly_1901_2019.nc")

# Creazione di un nuovo raster brick per le medie annuali
rqy <- raster::brick(ncol = ncol(r), nrow = nrow(r), nl = nlayers(r)/12,
                     xmn = extent(r)[1], xmx = extent(r)[2], 
                     ymn = extent(r)[3], ymx = extent(r)[4], 
                     crs = crs(r))

# Ciclo for per calcolare le medie annuali
for (i in 1:(nlayers(r)/12)) {
  start <- (i - 1) * 12 + 1
  end <- start + 11
  yearly_mean <- calc(r[[start:end]], mean)
  rqy[[i]] <- yearly_mean
}

# Salvataggio dati
years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
names(rqy) <- paste0("qr", years)
output_nc <- "Data/rsy.nc"
# Scrivi il RasterBrick in un file NetCDF
writeRaster(rqy, filename = output_nc, format = "CDF", overwrite = TRUE)




#############################################################################################################################
####  POINT DATA CONFLICT UPPSALA  ##########################################################################################
# Esempio con Nigeria, poi basta sostituire il nome
country <- "Nigeria"


file_path <- paste("Data_Raw/Conflict_Data/", country, ".csv", sep = "")
events <- read.csv(file_path)
# Seleziono soltanto le variabili che mi interessano
events <- events[, c("relid", "code_status","latitude" ,"longitude", "best_est")]
events <- events %>%
  rename(year = relid,
         type = code_status)
events <- mutate(events,
                 type = case_when(
                   type == 1 ~ "state",
                   type == 2 ~ "Nstate",
                   type == 3 ~ "onesided"
                 ))

output_folder <- "Data/Conflict"
output_file <- file.path(output_folder, paste0(country, ".csv"))
write.csv(events, file = output_file, row.names = FALSE)






































