# Il dataset 'gw_events_data' contine le variabili: CNTRY_NAME, ADMIN_REGION, geometry, yera, type, number, value
# Quindi per lo stato selezionato, per ogni anno, per ogni regione e per ogni tipo di conflitto abbiamo
# il valore del raster mediato sulla regione ed il numero di eventi.

# Il dataset 'gw_data_sc'contiene le variabili: CNTRY_NAME, region, geometry, value, year
# Quindi abbiamo, per lo stato selezionato, per ogni regione e per ogni anno il valore della media del raster selezionato
# all'interno delle regioni dello stato.
##############################################################################################################################
##############################################################################################################################

# Scegliere quale raster usare (rs, rt, rq)
r <- rs
a <- "rs"
# Selezionare il paese e anno
country <- "Nigeria"

##############################################################################################################################
##############################################################################################################################

# Subset dello shapefile per il paese selezionato
state <- subset(shp, CNTRY_NAME == country)    ## plot(state[,"geometry"])
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
gw_data_sc <- left_join(state, gw_data_sc, by=c("ADMIN_NAME"="region")) 
colnames(gw_data_sc)[colnames(gw_data_sc) == "ADMIN_NAME"] <- "region"

##############################################################################################################################
##############################################################################################################################

# Operazioni sui dati events
file_path <- paste("Data/Conflict/", country, ".csv", sep = "")
events <- read.csv(file_path)
events <- events %>%
  mutate(
    longitude = as.numeric(str_extract(stringr::str_extract(longitude, " \\d+\\.\\d+"), "\\d+\\.\\d+")))
# Elimino le righe con valori NA di latitudine e longitudine
events <- na.omit(events[, c("year", "type","latitude" ,"longitude","best_est")])
# Imposto lo stesso CRS dello shapefile sui punti
events <- st_as_sf(events, coords = c("longitude", "latitude"), crs = st_crs(state))
st_set_crs(events, st_crs(state))
events <- events %>%
  mutate(
    latitude = as.numeric(str_extract(geometry, "\\d+\\.\\d+")),
    longitude = as.numeric(str_extract(stringr::str_extract(geometry, " \\d+\\.\\d+"), "\\d+\\.\\d+"))
  )
events$geometry = NULL
events <- na.omit(events)
events = sf::st_as_sf(events, coords = c("longitude","latitude"), remove = FALSE)
sf::st_crs(events) = sf::st_crs(state)
intersection = sf::st_intersects(events, state)            
# set non-matched values to NA, these points are recorded in no province (outliers, miscoding, ...)
intersection[sapply(intersection, length) == 0] <- NA    
# merge region name to conflict data points
events$region <- state$ADMIN_NAME[unlist(intersection)]   
events$geometry = NULL
events <- events %>% group_by(year, region, type, latitude, longitude) %>% summarise(number_events = n())
colnames(events) <- c("year","region", "type", "latitude", "longitude","number")
# Ordino il dataset rispetto all'anno
events <- events[order(events$year),]

##############################################################################################################################
##############################################################################################################################
# Operazioni sui dati GW + events
gw_events_sc <- expand.grid(year = 1990:2022, region = unique(events$region),type=c("state","Nstate","onesided"))
gw_events_sc <- left_join(gw_events_sc, events, by=c("region", "year", "type"="type"))
gw_events_sc$number[is.na(gw_events_sc$number)] = 0 # assign a zero to each month/province where no data is observed
gw_events_sc$latitude = NULL ; gw_events_sc$longitude=NULL
filter <- gw_data_sc %>%
  filter(year > 1989)
gw_events_sc <- gw_events_sc %>%
  filter(year != 2020 & year != 2021 & year != 2022)
gw_events_sc <- left_join(gw_events_sc, filter[,c("year", "region","value")], by=c("year", "region"))
gw_events_sc <- left_join(state, gw_events_sc, by=c("ADMIN_NAME"="region")) 
gw_events_sc$geometry.y=NULL
st_geometry(gw_events_sc) <- "Geometry"


percorso_cartella <- paste0("Data/GW_Conflict/",country,"/")
if (!file.exists(percorso_cartella)) {
  dir.create(percorso_cartella, recursive = TRUE)
}
write_sf(gw_events_sc, paste0(percorso_cartella, country, "_gw_events_",a, ".shp"))
write.csv(events,paste0(percorso_cartella, country, "_events.csv"))













