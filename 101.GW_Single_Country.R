# Il dataset 'gw_data_sc'contiene le variabili: CNTRY_NAME, region, geometry, value, year
# Quindi abbiamo, per lo stato selezionato, per ogni regione e per ogni anno il valore della media del raster selezionato
# all'interno delle regioni dello stato.
# Il dataset 'gw_events_data' contine le variabili: year, region, geometry, type, number e value
# Quindi per lo stato selezionato, per ogni anno, per ogni regione e per ogni tipo di conflitto abbiamo
# il valore del raster mediato sulla regione ed il numero di eventi.




# Scegliere quale raster usare (rs, rt, rq)
r <- rs
# Selezionare il paese e anno
country <- "Nigeria"
y <- "1990"
# Selezionare le regioni per le serie temporali
R <- c("Borno", "Gombe", "Nasarawa", "Sokoto")
# R <- c(gw_data_t$region)  ## Se voglio vederle tutte insieme

##############################################################################################################################
##############################################################################################################################

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
gw_data_sc <- left_join(state, gw_data_sc, by=c("ADMIN_NAME"="region")) 
colnames(gw_data_sc)[colnames(gw_data_sc) == "ADMIN_NAME"] <- "region"


##############################################################################################################################
##############################################################################################################################

# Plot mappa anno scelto
data <- subset(gw_data_sc, year == y)
ggplot(data, aes(fill=value)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)



# Plot serie temporali per regioni scelte
ggplot(subset(gw_data_sc, region %in% R), 
       aes(year, value, fill=value, col=value)) +   
  geom_hline(yintercept=0) +                  
  geom_hline(yintercept=-1.5, lty=3) +     
  facet_wrap(region~., ncol=2) +        
  theme_bw() +         
  theme(strip.background=element_rect(fill="white")) +          
  ylab("") +                                                                
  xlab("") +                                                                       
  geom_col() +                                                                      
  scale_fill_viridis_c(option="viridis", end  = 0.8) +                              
  scale_color_viridis_c(option="viridis", end = 0.8)





#########################################################################################
#########################################################################################

# Operazioni sui dati events
file_path <- paste("Data/Conflict/", country, ".csv", sep = "")
events <- read.csv(file_path)
state <- subset(shp, CNTRY_NAME == country)    ## plot(state[,"geometry"])
events <- events %>%
  mutate(
    longitude = as.numeric(str_extract(stringr::str_extract(longitude, " \\d+\\.\\d+"), "\\d+\\.\\d+"))
  )
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


# Operazioni sui dati GW
gw_events_data <- expand.grid(year = 1990:2022, region = unique(events$region),type=c("state","Nstate","onesided"))
gw_events_data <- left_join(gw_events_data, events, by=c("region", "year", "type"="type"))
gw_events_data$number[is.na(gw_events_data$number)] = 0 # assign a zero to each month/province where no data is observed
gw_events_data$latitude = NULL ; gw_events_data$longitude=NULL
gw_data_m <- gw_data_sc %>%
  filter(year > 1989)
gw_events_data <- gw_events_data %>%
  filter(year != 2020 & year != 2021 & year != 2022)
gw_events_data <- left_join(gw_events_data, gw_data_m[,c("year", "region","value")], by=c("year", "region"))
gw_events_data <- left_join(state, gw_events_data, by=c("ADMIN_NAME"="region")) 


#################################################################################################
#################################################################################################

data <- subset(gw_events_data, year == 2019)
data <- subset(gw_events_data, type == "state")
ggplot(data, aes(fill = number)) +  
  # plots the sf object / shapefile
  geom_sf() + 
  # nice theme
  theme_bw() +         
  # viridis color palette
  scale_fill_viridis_c(option="inferno", end=0.8) 


years <- c(1990, 1991, 1992, 1993)  
data_selected <- subset(state, year %in% years)
ggplot(data, aes(fill = number)) +  
  # plots the sf object / shapefile
  geom_sf() + 
  # nice theme
  theme_bw() +         
  # viridis color palette
  scale_fill_viridis_c(option="viridis", end=0.8) 


ggplot(state) +           
  geom_sf(fill = NA, col = "black") +  
  # plot points for each event, using longitude, latitude as x and y, set shape and color of points using variable "event type" (which is either protests or riots)
  geom_point(data = events, aes(longitude, latitude, color = factor(type)), size = .5) +
  # Assegna manualmente i colori ai valori di code_status
  scale_color_manual(values = c("state" = "red", "Nstate" = "blue", "onesided" = "green"))+
  theme_bw() +                     
  # white background for variable names
  theme(strip.background = element_rect(fill="white"))
























