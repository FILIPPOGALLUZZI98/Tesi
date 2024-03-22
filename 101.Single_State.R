#################################################################################################
####  CARICARE I DATI  #########################################################################
# Upload dei dati shapefile + settaggio coordinate
shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 

# Upload dati delle medie annuali (non diretti ISIMIP3a) + settaggio coordinate
r <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")
proj4string(r) <- raster::crs(shp)


#################################################################################################
####  PLOT FOR SINGLE STATE  ####################################################################
# Seleziono il paese
state <- subset(shp, CNTRY_NAME == "Italy")
# Media dei valori del raster sulle regioni 
gw_data <- exactextractr::exact_extract(r, state, fun="mean")
gw_data$region <- state$ADMIN_NAME
# Rinomino gli anni
for (year in 1901:2019) {
  col_name <- paste0("gw_", year)
  state[[col_name]] <- gw_data[[paste0("mean.X", year - 1900)]]
}

# Plot anno 2000
ggplot(state, aes(fill=gw_2000)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)





























