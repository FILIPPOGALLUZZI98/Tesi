# Lista contenente i valori del raster mediati nelle singole regioni di tutti gli stati uno per uno
# Creo una lista dove ci sono tutti gli stati già uniti con valori di GW

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
  scale_fill_viridis_c(option="viridis", end=0.8)






