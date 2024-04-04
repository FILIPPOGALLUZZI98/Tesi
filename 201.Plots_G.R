

dati_filtrati <- gw_g %>%
  filter(year == 1920) 

ggplot() +
  geom_sf(data = shp, aes(fill = region)) +
  geom_sf(data = dati_filtrati, aes(color = value), size = 2, show.legend = "point") +
  scale_color_viridis_c() +  # Scala dei colori per i valori 'value'
  scale_fill_manual(values = "transparent") +  # Trasparenza per le geometrie dello shapefile
  labs(title = "Mappa del valore per regione (1920)",
       color = "Value")
