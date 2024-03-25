shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
# Rimuovo le regioni che danno geometry error
elementi <- c(34, 381, 2070, 419, 420, 2071, 643, 770, 868, 930, 2072, 1065, 
              1105, 1162, 1542, 1548, 1578, 1824, 1968, 2073)
shp<- shp[-elementi,]
# Raster è quello modificato da me medie annuali e non mensili
r <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")  ## groundwstrg
## r <- raster::brick("GW_Data/ISIMIP3a/twsy.nc")  ## total water storage
## r <- raster::brick("GW_Data/ISIMIP3a/qry.nc")  ## runoff (??)
proj4string(r) <- raster::crs(shp)

#################################################################################################
#################################################################################################

country <- "Nigeria"

file_path <- paste("GW_Data/Conflict_Data/", country, ".csv", sep = "")
events <- read.csv(file_path)
state <- subset(shp, CNTRY_NAME == country)
gw_data <- exactextractr::exact_extract(r, state, fun="mean")
gw_data$region <- state$ADMIN_NAME

# Seleziono soltanto le variabili che mi interessano
events <- events[, c("relid", "code_status","type_of_violence","latitude" ,"longitude")]




# Per impostare lo stesso crs devo prima separare i valori della longitudine dalla latitudine
events <- events %>%
  mutate(
    longitude = as.numeric(str_extract(stringr::str_extract(longitude, " \\d+\\.\\d+"), "\\d+\\.\\d+"))
  )
# Elimino le righe con valori NA di latitudine e longitudine
events <- na.omit(events[, c("relid", "code_status","type_of_violence","latitude" ,"longitude")])
# Imposto lo stesso CRS dello shapefile sui punti
events <- st_as_sf(events, coords = c("longitude", "latitude"), crs = st_crs(state))
st_set_crs(events, st_crs(state))
events <- events %>%
  mutate(
    latitude = as.numeric(str_extract(geometry, "\\d+\\.\\d+")),
    longitude = as.numeric(str_extract(stringr::str_extract(geometry, " \\d+\\.\\d+"), "\\d+\\.\\d+"))
  )
events <- subset(events, select = -geometry)





# events è il dataset dei punti del paese che ho selezionato nel giusto crs
ggplot(state) +           
  # plot the outlines of the shapefile of italy (using black borders and transparent filling)
  geom_sf(fill = NA, col = "black") +  
  # plot points for each event, using longitude, latitude as x and y, set shape and color of points using variable "event type" (which is either protests or riots)
  geom_point(data = events, aes(longitude, latitude, color = factor(code_status)), size = .5) +
  # Assegna manualmente i colori ai valori di code_status
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green"))
  theme_bw() +                     
  # hide legend
  theme(legend.position = "none") +          
  # set color scheme
  scale_color_viridis_d(end = 0.7,begin=0.2,option="inferno") +    
  # white background for variable names
  theme(strip.background = element_rect(fill="white"))
