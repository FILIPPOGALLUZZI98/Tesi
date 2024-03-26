# Scegliere quale raster usare (rs, rt, rq)
r <- rs
# Selezionare il paese e anno
country <- "Nigeria"
y <- 1990





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


state <- left_join(state, gw_data_sc, by=c("ADMIN_NAME"="region")) 

# Plot anno scelto
data <- subset(state, year == y)
ggplot(data, aes(fill=value)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)
