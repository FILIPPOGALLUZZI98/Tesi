suppressPackageStartupMessages({
  library(sf)              ## useful for spatial manipulations
  library(sp)              ## useful for spatial manipulations
  library(raster)          ## useful for working with raster data
  library(ncdf4)           ## useful for working with raster data
  library(exactextractr)   ## useful for extracting data from raster files
  library(dplyr)           ## useful for merging data sets
  library(reshape2)        ## useful for manipulating data sets
  library(ggplot2)         ## useful for data visualization
  library(ggrepel)         ## useful for labeling point plots in ggplot2
  library(lubridate)
  library(zoo)   
})


shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
r <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")
proj4string(r) <- raster::crs(shp)


################################################################################################
################################################################################################
gw_data <- list()
state <- list()

nomi <-list("Argentina", "Italy", "Austria", "Germany")

for (i in seq_along(nomi)) {
  a <- subset(shp, CNTRY_NAME == nomi[i])
  state <- append(state, list(a))
}
## plot(state[[1]][,"geometry"])  


for (i in 0:4) {
  tryCatch({
    b <- exactextractr::exact_extract(r, state[[i]], fun="mean")
    b$region <- state[[i]]$ADMIN_NAME
    gw_data <- append(gw_data, list(b))
  }, error = function(e) {
    # Gestisci l'errore qui, puoi anche stampare un messaggio di avviso
    cat("Errore durante l'elaborazione dell'elemento", i, ":", conditionMessage(e), "\n")
  })
}


# Plot anno 2000
ggplot(state[[2]], aes(fill=gw_data[2]$mean.X117)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)

################################################################################################
################################################################################################
gw_data <- list()
state <- list()

for (i in seq_along(shp$CNTRY_NAME)) {
  a <- subset(shp, CNTRY_NAME == shp$CNTRY_NAME[i])
  state <- append(state, list(a))
}


for (i in seq_along(state)) {
  tryCatch({
    b <- exactextractr::exact_extract(r, state[[i]], fun="mean")
    gw_data <- append(gw_data, list(b))
    gw_data[[i]]$region <- state[[i]]$ADMIN_NAME
  }, error = function(e) {
    # Gestisci l'errore qui, puoi anche stampare un messaggio di avviso
    cat("Errore durante l'elaborazione dell'elemento", i, ":", conditionMessage(e), "\n")
  })
}


# Plot anno 2000
ggplot(state[[100]], aes(fill=gw_data[[100]]$mean.X117)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)
















