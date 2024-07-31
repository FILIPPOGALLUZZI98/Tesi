file_tiff <- "GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0.tif"

# Carica il file raster
pop_t <- raster(file_tiff)
shp <- st_read("^Data/separate/shp/shp.shp")
proj4string(pop_t) <- raster::crs(shp)



factor <- 10
pop_t2 <- aggregate(pop_t, fact=factor, fun=mean)


pop <- exactextractr::exact_extract(pop_t2, shp, fun="mean")
pop <- data.frame(pop = pop)

pop$region <- shp$region; pop$country <- shp$country; pop$orig<-shp$GEOLEVEL1 
pop$year <- 1990

ita <- subset(shp, country=="Argentina")
italia <- subset(pop, country=="Argentina")

merged_data <- merge(ita, italia, by.x = "region", by.y = "region")

# Plotta i dati con ggplot2
ggplot(data = merged_data) +
  geom_sf(aes(fill = pop)) +
  scale_fill_viridis_c() +  # Opzione di colore, puoi cambiarla in base alle tue preferenze
  theme_minimal() 
