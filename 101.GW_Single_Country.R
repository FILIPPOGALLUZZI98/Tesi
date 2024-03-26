# Il dataset 'state'contiene le variabili: CNTRY_NAME, ADMIN_NAME, geometry, value, year
# Quindi abbiamo, per lo stato selezionato, per ogni regione e per ogni anno il valore della media del raster selezionato
# all'interno delle regioni dello stato

##############################################################################################################################
##############################################################################################################################

# Scegliere quale raster usare (rs, rt, rq)
r <- rs
# Selezionare il paese e anno
country <- "Nigeria"
y <- 1990




# Subset dello shapefile per il paese selezionato
state <- subset(shp, CNTRY_NAME == country)
# Media dei valori del raster sulle regioni 
gw_data_sc <- exactextractr::exact_extract(r, state, fun="mean")
# Aggiungo una colonna region al file gw_data_state
gw_data_sc$region <- state$ADMIN_NAME

gw_data_sc <- reshape2::melt(gw_data_sc, id.vars="region")
anni <- 1901:2019
gw_data_sc <- gw_data_sc %>%
  group_by(region) %>%
  mutate(year = anni)
gw_data_sc$variable=NULL


state <- left_join(state, gw_data_sc, by=c("ADMIN_NAME"="region")) 

# Plot anno scelto
data <- subset(state, year == y)
ggplot(data, aes(fill=value)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)


##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################


# Scegliere quale raster usare (rs, rt, rq)
r <- rs
# Selezionare il paese e anno
country <- "Nigeria"
y <- 1990




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
x <- which(nomi == country)
ggplot(state[[x]], aes(fill=gw_data[[x]][[paste0("mean.X", y - 1900)]])) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill = paste("groundwstrg", country, y)) +
  scale_fill_viridis_c(option="viridis", end=0.8)
















