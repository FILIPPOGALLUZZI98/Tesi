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

















