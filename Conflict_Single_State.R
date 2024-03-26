# Dataset dei conflitti dei singoli stati da Uppsala

# Seleziono lo stato
country <- "Nigeria"


file_path <- paste("GW_Data/Conflict_Data/", country, ".csv", sep = "")
events <- read.csv(file_path)
state <- subset(shp, CNTRY_NAME == country)    ## plot(state[,"geometry"])
state$BPL_CODE=NULL; state$CNTRY_CODE=NULL; state$GEOLEVEL1=NULL
gw_data <- exactextractr::exact_extract(r, state, fun="mean")
gw_data$region <- state$ADMIN_NAME
gw_data_m <- reshape2::melt(gw_data, id.vars="region")

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


ggplot(state) +           
  geom_sf(fill = NA, col = "black") +  
  # plot points for each event, using longitude, latitude as x and y, set shape and color of points using variable "event type" (which is either protests or riots)
  geom_point(data = events, aes(longitude, latitude, color = factor(type)), size = .5) +
  # Assegna manualmente i colori ai valori di code_status
  scale_color_manual(values = c("state" = "red", "Nstate" = "blue", "onesided" = "green"))+
  theme_bw() +                     
  # white background for variable names
  theme(strip.background = element_rect(fill="white"))

#################################################################################################
#################################################################################################
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


#################################################################################################
#################################################################################################

full <- expand.grid(year = 1990:2022, region = unique(events$region),type=c("state","Nstate","onesided"))
full <- left_join(full, events, by=c("region", "year", "type"="type"))
full$number[is.na(full$number)] = 0 # assign a zero to each month/province where no data is observed
full$latitude = NULL ; full$longitude=NULL

gw_data_m$year <- lubridate::year(zoo::as.Date(zoo::as.yearmon(substr(as.character(gw_data_m$variable), 7, 13), "%Y.%m")))  # extract the year from the data
anni <- 1901:2019
nomi_anni <- paste("X", anni - 1900, sep = "")
gw_data_m <- rename(gw_data_m, !!paste0("mean.", nomi_anni) := variable)
gw_data_m <- gw_data_m %>%
  group_by(region) %>%
  mutate(year = anni)
gw_data_m$variable=NULL
gw_data_m <- gw_data_m %>%
  filter(year > 1989)

full <- full %>%
  filter(year != 2020 & year != 2021 & year != 2022)

full <- left_join(full, gw_data_m[,c("year", "region","value")], by=c("year", "region"))


#################################################################################################
#################################################################################################

full1 <- full
full1$type=NULL
state <- left_join(state, full1, by=c("ADMIN_NAME"="region")) 


data <- subset(state, year == 2019)
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












