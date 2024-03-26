# Il dataset 'gw_data_sc'contiene le variabili: CNTRY_NAME, region, geometry, value, year
# Quindi abbiamo, per lo stato selezionato, per ogni regione e per ogni anno il valore della media del raster selezionato
# all'interno delle regioni dello stato



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














