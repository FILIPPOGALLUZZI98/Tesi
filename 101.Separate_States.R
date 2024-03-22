shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
# Rimuovo le regioni che danno geometry error
elementi <- c(34, 381, 2070, 419, 420, 2071, 643, 770, 868, 930, 2072, 1065, 
              1105, 1162, 1542, 1548, 1578, 1824, 1968, 2073)
shp<- shp[-elementi,]
# Raster è quello modificato da me medie annuali e non mensili
r <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")
proj4string(r) <- raster::crs(shp)



gw_data <- list()
state <- list()
nomi <- unique(shp$CNTRY_NAME)

for (i in seq_along(nomi)) {
  a <- subset(shp, CNTRY_NAME == nomi[i])
  state <- append(state, list(a))
}

for (i in 1:283) {
  b <- exactextractr::exact_extract(r, state[[i]], fun="mean")
  b$region <- state[[i]]$ADMIN_NAME
  gw_data <- append(gw_data, list(b))
}


# Selezionare il nome del paese che voglio plottare e l'anno
y <- 1901
country <- "Nigeria"
x <- which(nomi == country)

ggplot(state[[x]], aes(fill=gw_data[[x]][[paste0("mean.X", y - 1900)]])) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill = paste("groundwstrg", country, y)) +
  scale_fill_viridis_c(option="inferno", end=0.8)






