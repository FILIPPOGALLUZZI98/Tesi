# Il dataset 'gw_events_data' contine le variabili: CNTRY_NAME, ADMIN_REGION, geometry, yera, type, number, value
# Quindi per lo stato selezionato, per ogni anno, per ogni regione e per ogni tipo di conflitto abbiamo
# il valore del raster mediato sulla regione ed il numero di eventi.

# Il dataset 'gw_data_sc'contiene le variabili: CNTRY_NAME, region, geometry, value, year
# Quindi abbiamo, per lo stato selezionato, per ogni regione e per ogni anno il valore della media del raster selezionato
# all'interno delle regioni dello stato.

##############################################################################################################################
##############################################################################################################################
# Select the country
country <- "Brazil"
# Select the raster
rast <- "rs"



suppressPackageStartupMessages({
  library(sf)              ## useful for spatial manipulations
  library(sp)              ## useful for spatial manipulations
  library(plyr )
  library(raster)          ## useful for working with raster data
  library(ncdf4)           ## useful for working with raster data
  library(exactextractr)   ## useful for extracting data from raster files
  library(dplyr)           ## useful for merging data sets
  library(stringr)
  library(reshape2)        ## useful for manipulating data sets
  library(ggplot2)         ## useful for data visualization
  library(ggrepel)         ## useful for labeling point plots in ggplot2
  library(lubridate)
  library(zoo)   
  library(foreign)
})

shp <- sf::read_sf("Data/Shapefile/shapefile.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
r <- raster::brick(paste0("Data/GW/",rast,"y.nc")); proj4string(r) <- raster::crs(shp)
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
# gw_data_sc <- left_join(state, gw_data_sc, by=c("ADMIN_NAME"="region")) 
# colnames(gw_data_sc)[colnames(gw_data_sc) == "ADMIN_NAME"] <- "region"


#############################################################################################################################
####  POINT DATA CONFLICT UPPSALA  ##########################################################################################
# DA OTTIMIZZARE, RIMUOVERE IL SALVATAGGIO INTERMEDIO CHE è INUTILE

file_path <- paste("Data_Raw/Conflict_Data/", country, ".csv", sep = "")
events <- read.csv(file_path)
# Seleziono soltanto le variabili che mi interessano
events <- events[, c("relid", "code_status","latitude" ,"longitude", "best_est")]
events <- events %>%
  rename(year = relid,
         type = code_status)
events <- mutate(events,
                 type = case_when(
                   type == 1 ~ "state",
                   type == 2 ~ "Nstate",
                   type == 3 ~ "onesided"
                 ))

output_folder <- "Data/Conflict"
output_file <- file.path(output_folder, paste0(country, ".csv"))
write.csv(events, file = output_file, row.names = FALSE)

##############################################################################################################################
####  EVENTS INTERSECTION  ####################################################################################################


# Operazioni sui dati events
file_path <- paste("Data/Conflict/", country, ".csv", sep = "")
events <- read.csv(file_path)
events$latitude=NULL
events$latitude <- as.numeric(str_extract(events$longitude, "(?<=POINT \\()[0-9.-]+"))
events$longitude <- as.numeric(str_extract(events$longitude, "(?<=\\s)[0-9.-]+(?=\\))"))
events <- na.omit(events[, c("year", "type","latitude" ,"longitude","best_est")])
events = sf::st_as_sf(events, coords = c("latitude","longitude"), remove = FALSE)
sf::st_crs(events) = sf::st_crs(state)
events$longitude <- st_coordinates(events)[, "X"]
events$latitude <- st_coordinates(events)[, "Y"]
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
gw_data_sc <- gw_data_sc %>%
  filter(year > 1989)

gw_events_sc <- expand.grid(year = 1990:2022, region = unique(events$region),type=c("state","Nstate","onesided"))
gw_events_sc <- left_join(gw_events_sc, events, by=c("region", "year", "type"="type"))
gw_events_sc$number[is.na(gw_events_sc$number)] = 0 # assign a zero to each month/province where no data is observed
gw_events_sc$latitude = NULL ; gw_events_sc$longitude=NULL
gw_events_sc <- gw_events_sc %>%
  filter(year != 2020 & year != 2021 & year != 2022)
gw_events_sc <- left_join(gw_events_sc, gw_data_sc[,c("year", "region","value")], by=c("year", "region"))
# st_geometry(gw_events_sc) <- "geometry"


percorso_cartella <- paste0("Data/GW_Conflict/",country,"/")
if (!file.exists(percorso_cartella)) {
  dir.create(percorso_cartella, recursive = TRUE)
}


##############################################################################################################################
##############################################################################################################################
# Scrittura datasets

write.csv(gw_data_sc, paste0(percorso_cartella, country, "_gw_",rast, ".csv"))
write.csv(gw_events_sc, paste0(percorso_cartella, country, "_gw_events_",rast, ".csv"))
write.csv(events,paste0(percorso_cartella, country, "_events.csv"))













