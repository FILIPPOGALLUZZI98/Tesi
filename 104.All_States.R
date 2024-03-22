#################################################################################################
####  CARICARE E SISTEMARE I DATI  ##############################################################

# Upload dei dati shapefile + settaggio coordinate
shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 

# Upload dati delle medie annuali (non diretti ISIMIP3a) + settaggio coordinate
r <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")
proj4string(r) <- raster::crs(shp)

# Voglio rimuovere soltanto le singole regioni degli stati che hanno geometry EMPTY, in modo da
# poter usare le regioni rimanenti
# Cerco il numero della posizione delle regioni dei paesi che hanno geometry EMPTY
elementi <- c(34, 381, 2070, 419, 420, 2071, 643, 770, 868, 930, 2072, 1065, 
              1105, 1162, 1542, 1548, 1578, 1824, 1968, 2073)
# Rimuovo questi elementi da shp
shp<- shp[-elementi,]


#################################################################################################
####  PLOT PER TUTTI GLI STATI SEPARATI  ########################################################

gw_data <- list()
state <- list()
nomi <- unique(shp$CNTRY_NAME)

for (i in seq_along(nomi)) {
  a <- subset(shp, CNTRY_NAME == nomi[i])
  state <- append(state, list(a))
}

for (i in 1:264) {
  b <- exactextractr::exact_extract(r, state[[i]], fun="mean")
  b$region <- state[[i]]$ADMIN_NAME
  gw_data <- append(gw_data, list(b))
}

# Seleziono il nome del paese che voglio plottare
country <- "Nigeria"
x <- which(nomi == country)
# Seleziono l'anno ricordando che mean.X1<-->1901 e così via
ggplot(state[[x]], aes(fill=gw_data[[x]]$mean.X1)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)






























