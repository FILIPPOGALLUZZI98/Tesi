# Upload dei dati shapefile + settaggio coordinate
shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 

# Upload dati delle medie annuali (non diretti ISIMIP3a) + settaggio coordinate
r <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")
proj4string(r) <- raster::crs(shp)


#################################################################################################
####  PLOT FOR SINGLE STATE  ####################################################################
# Seleziono il paese (ci sono alcuni paesi che danno geometry error, perché alcune geometry sono vuote)
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


################################################################################################
####  ESEMPIO ALGORITMO PER POCHI STATI PER VEDERE SE FUNZIONA  ################################
gw_data <- list()
state <- list()
nomi <- c("Italy","Austria","Germany")  ## Alcuni di quelli che NON danno geometry error

for (i in seq_along(nomi)) {
  a <- subset(shp, CNTRY_NAME == nomi[i])
  state <- append(state, list(a))
}

for (i in 1:3) {
    b <- exactextractr::exact_extract(r, state[[i]], fun="mean")
    b$region <- state[[i]]$ADMIN_NAME
    gw_data <- append(gw_data, list(b))
}

# Plot anno 2000 per Italia
ggplot(state[[1]], aes(fill=gw_data[[1]]$mean.X117)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)


################################################################################################
####  RIMOZIONE STATI CON ERRORE  ##############################################################
gw_data <- list()
state <- list()
nomi <- unique(shp$CNTRY_NAME)

# Attraverso 'trycatch' posso vedere quali paesi mi danno errore e posso quindi rimuoverli 
# Devo rimuovere gli stati che danno errore nel momento del merging dei dati
posizioni_da_rimuovere <- c(10, 63, 66, 72, 74, 99, 108, 122, 134, 145, 161, 163, 
                            175, 207, 209, 211, 233, 242, 245)
nomi_rimossi <- nomi[posizioni_da_rimuovere]
nomi <- nomi[-posizioni_da_rimuovere]
print(nomi_rimossi)

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
country <- "Italy"
x <- which(nomi == country)
# Seleziono l'anno ricordando che mean.X1<-->1901 e così via
ggplot(state[[x]], aes(fill=gw_data[[x]]$mean.X117)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)


#################################################################################################
#################################################################################################
#################################################################################################
####  RIMOZIONE SOLO REGIONI CON ERRORE  ########################################################

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
























