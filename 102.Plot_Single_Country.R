# Select the country
country <- "Myanmar"
# Select the year
y <- "2000"
# Select the raster
r <- "rs"

path <- paste0("Data/GW_Conflict/", country, "/")
data_gw_events <- sf::read_sf(paste0(path, country,"_gw_events_", r,".shp"))
events <-read.csv(paste0(path, country, "_events.csv"))
######################################################################################################

# Plot mappa GW anno scelto
data_year <- subset(data_gw_events, year == y)
ggplot(data_year, aes(fill=value)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)

######################################################################################################

# Plot serie temporali GW per regioni scelte
# Selezionare le regioni per le serie temporali
# R <- c("", "", "", "")
R <- c(data_gw_events$ADMIN_NAME)  ## Se voglio vederle tutte insieme

ggplot(subset(data_gw_events, ADMIN_NAME %in% R), 
       aes(year, value, fill=value, col=value)) +   
  facet_wrap(ADMIN_NAME~., ncol=6) +        
  theme_bw() +         
  theme(strip.background=element_rect(fill="white")) +          
  ylab("") +                                                                
  xlab("") +                                                                       
  geom_col() +                                                                      
  scale_fill_viridis_c(option="viridis", end  = 0.8) +                              
  scale_color_viridis_c(option="viridis", end = 0.8)


######################################################################################################
# Plot della timeseries in una regione dei conflitti Nstate

reg <- "Kayin"
data <- subset(data_gw_events,ADMIN_NAME==reg)
data <- subset(data, type=="Nstate")


ggplot(data = data, aes(x = year, y = count)) +
  geom_point() +  # Aggiunge i punti
  geom_line() +   # Aggiunge la linea
  labs(x = "Year", y = "Count") +
  ggtitle("Somma dei valori 'number' per anno")


######################################################################################################
# Plot della timeseries dei conflitti in una regione state+Nstate+onesided

reg <- "Kayin"
data <- subset(data_gw_events,ADMIN_NAME==reg)
agg_data <- data %>%
  group_by(year, ADMIN_NAME) %>%
  summarise(count = sum(number))

ggplot(data = agg_data, aes(x = year, y = count)) +
  geom_point() +  # Aggiunge i punti
  geom_line() +   # Aggiunge la linea
  labs(x = "Year", y = "Count") +
  ggtitle("Somma dei valori 'number' per anno")

######################################################################################################
# Da togliere state e mettere data_gw_events

ggplot(data_gw_events) +           
  geom_sf(fill = NA, col = "black") +  
  geom_point(data = events, aes(longitude, latitude, color = factor(type)), size = .5) +
  # Assegna manualmente i colori ai valori di code_status
  scale_color_manual(values = c("state" = "red", "Nstate" = "blue", "onesided" = "green"))+
  theme_bw() +                     
  # white background for variable names
  theme(strip.background = element_rect(fill="white"))





















