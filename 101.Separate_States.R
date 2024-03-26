#################################################################################################
####  SINGLE STATES  ############################################################################
# Valori del raster mediati nelle singole regioni di uno stato scelto

state <- subset(shp, CNTRY_NAME == "Nigeria")

state$BPL_CODE=NULL; state$CNTRY_CODE=NULL; state$GEOLEVEL1=NULL
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

#################################################################################################
####  ALGORITHM  ################################################################################
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






