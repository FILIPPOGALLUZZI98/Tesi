libia <- read.csv("GW_Data/Conflict_Data/Libia.csv")
state <- subset(shp, CNTRY_NAME == "Libya")
libia <- libia[, c("relid", "code_status","type_of_violence","latitude" ,"longitude")]


# Per estrarre i valori della longitudine 
lib <- libia %>%
  mutate(
    longitude = as.numeric(str_extract(stringr::str_extract(longitude, " \\d+\\.\\d+"), "\\d+\\.\\d+"))
  )
# Elimino le righe con valori NA di latitudine e longitudine
lib1 <- na.omit(lib[, c("latitude", "longitude")])  ## Mi elimina tutte le colonne tranne lon e lat perché???


# Imposto lo stesso CRS dello shapefile sui punti
lib <- st_as_sf(lib, coords = c("longitude", "latitude"), crs = st_crs(state))
st_set_crs(sf_df, st_crs(state))
lib <- sf_df %>%
  mutate(
    latitude = as.numeric(str_extract(geometry, "\\d+\\.\\d+")),
    longitude = as.numeric(str_extract(stringr::str_extract(geometry, " \\d+\\.\\d+"), "\\d+\\.\\d+"))
  )


ggplot(state) +           
  # plot the outlines of the shapefile of italy (using black borders and transparent filling)
  geom_sf(fill = NA, col = "black") +  
  # plot points for each event, using longitude, latitude as x and y, set shape and color of points using variable "event type" (which is either protests or riots)
  geom_point(data = lib, aes(longitude, latitude), size=.1, col="red") + 
  # set nice theme
  theme_bw() +                     
  # hide legend
  theme(legend.position = "none") +          
  # set color scheme
  scale_color_viridis_d(end = 0.7,begin=0.2,option="inferno") +    
  # white background for variable names
  theme(strip.background = element_rect(fill="white"))
