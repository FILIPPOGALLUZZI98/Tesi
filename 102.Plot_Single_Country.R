# Plot mappa anno scelto
data <- subset(gw_data_sc, year == y)
ggplot(data, aes(fill=value)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)

######################################################################################################

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

######################################################################################################

ggplot(state) +           
  geom_sf(fill = NA, col = "black") +  
  # plot points for each event, using longitude, latitude as x and y, set shape and color of points using variable "event type" (which is either protests or riots)
  geom_point(data = events, aes(longitude, latitude, color = factor(type)), size = .5) +
  # Assegna manualmente i colori ai valori di code_status
  scale_color_manual(values = c("state" = "red", "Nstate" = "blue", "onesided" = "green"))+
  theme_bw() +                     
  # white background for variable names
  theme(strip.background = element_rect(fill="white"))

data <- subset(gw_events_data, year == 2019)
data <- subset(gw_events_data, type == "state")
ggplot(data, aes(fill = number)) +  
  # plots the sf object / shapefile
  geom_sf() + 
  # nice theme
  theme_bw() +         
  # viridis color palette
  scale_fill_viridis_c(option="inferno", end=0.8) 



















