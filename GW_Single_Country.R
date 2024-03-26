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





# Rinomino gli anni
for (year in 1901:2019) {
  col_name <- paste0("gw_", year)
  state[[col_name]] <- gw_data_sc[[paste0("mean.X", year - 1900)]]
}

# Plot anno 2000
ggplot(state, aes(fill=gw_2000)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)
