# Select the country
country <- "Nigeria"
# Select the year
y <- "1901"



######################################################################################################
######################################################################################################

path <- paste0("Data/GW_Conflict/", country, "/", country,"_gw_events.shp")
data_gw_events <- sf::read_sf(path)

######################################################################################################

# Plot mappa anno scelto
data <- subset(data_gw_events, year == y)
ggplot(data, aes(fill=value)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)

######################################################################################################

# Plot serie temporali per regioni scelte
# Selezionare le regioni per le serie temporali
R <- c("Borno", "Gombe", "Nasarawa", "Sokoto")
# R <- c(data_gw_events$ADMIN_NAME)  ## Se voglio vederle tutte insieme

ggplot(subset(data_gw_events, ADMIN_NAME %in% R), 
       aes(year, value, fill=value, col=value)) +   
  facet_wrap(ADMIN_NAME~., ncol=2) +        
  theme_bw() +         
  theme(strip.background=element_rect(fill="white")) +          
  ylab("") +                                                                
  xlab("") +                                                                       
  geom_col() +                                                                      
  scale_fill_viridis_c(option="viridis", end  = 0.8) +                              
  scale_color_viridis_c(option="viridis", end = 0.8)

######################################################################################################
# Da togliere state e mettere data_gw_events

ggplot(data_gw_events) +           
  geom_sf(fill = NA, col = "black") +  
  # plot points for each event, using longitude, latitude as x and y, set shape and color of points using variable "event type" (which is either protests or riots)
  geom_point(data = events, aes(longitude, latitude, color = factor(type)), size = .5) +
  # Assegna manualmente i colori ai valori di code_status
  scale_color_manual(values = c("state" = "red", "Nstate" = "blue", "onesided" = "green"))+
  theme_bw() +                     
  # white background for variable names
  theme(strip.background = element_rect(fill="white"))

data <- subset(data_gw_events, year == 2019)
data <- subset(data_gw_events, type == "state")
ggplot(data, aes(fill = number)) +  
  # plots the sf object / shapefile
  geom_sf() + 
  # nice theme
  theme_bw() +         
  # viridis color palette
  scale_fill_viridis_c(option="inferno", end=0.8) 



















