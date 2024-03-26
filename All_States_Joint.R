# Unisco direttamente i valori di GW direttamente sulle regioni
gw_data <- exactextractr::exact_extract(r, shp, fun="mean")
gw_data$region <- shp$ADMIN_NAME


# Selezionare l'anno
y <- 1901

ggplot(shp, aes(fill=gw_data[[paste0("mean.X", y - 1900)]])) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill=paste("groundwstrg", y)) +
  scale_fill_viridis_c(option="viridis", end=0.8)




















