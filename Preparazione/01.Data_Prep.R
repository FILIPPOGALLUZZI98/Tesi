#################################################################################################
####  SHAPEFILE  ############################################################################

shp <- sf::read_sf("^Data_Raw/world_geolev1_2021/world_geolev1_2021.shp")

# Rimuovo le variabili che non mi servono
shp$BPL_CODE <- NULL; shp$CNTRY_CODE <- NULL; shp$GEOLEVEL1 <- NULL
# Rimuovo le regioni che danno geometry error
empty <- st_is_empty(shp)
shp <- shp[!empty, ]
# Per vedere quali regioni sono state rimosse
nomi_geometrie_vuote_rimosse <- rownames(shp)[empty]
print(nomi_geometrie_vuote_rimosse)

# Salvo il dataset risultante
st_write(shp, "^Data/Shapefile/shp.gpkg")

######## dati_shapefile <- st_read("^Data/Shapefile/nome_file_shapefile.gpkg")

#################################################################################################
####  GROUNDWATER STORAGE YEAR AVERAGE  #########################################################
r <- raster::brick("^Data_Raw/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_groundwstor_global_monthly_1901_2019.nc")

media_annuale <- lapply(1:119, function(i) {
  anno_iniziale <- (i - 1) * 12 + 1
  anno_finale <- i * 12
  media <- mean(r[[anno_iniziale:anno_finale]])
  return(media)
})
rsy <- brick(media_annuale)

# Salvataggio dati
years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
names(rsy) <- paste0("gw", years)
output_nc <- "^Data/GW/rsy.nc"
# Scrivi il RasterBrick in un file NetCDF
writeRaster(rsy, filename = output_nc, format = "CDF", overwrite = TRUE)














































