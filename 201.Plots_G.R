suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

shp <- st_read("^Data/shp/shp.shp")
events <-read.csv("^Data/Global_events.csv")
data_gw <- read.csv("^Data/Global_gws.csv")
##  data_gw_events <- read.csv("^Data/Global_gws_events.csv")


######################################################################################################
# Plot GW map for a selected year and country
y="2002"; paese="Nigeria"

data <- subset(data_gw, year == y & country == paese)
state <- subset(shp, country==paese)
plot_mappa <- ggplot() +
  geom_sf(data = state, aes(fill = region), color = "black", size = 0.2) +
  geom_sf(data = data, aes(fill = value), color = "black", size = 0.2) +
  scale_fill_viridis_c() +
  labs(title = paste("Mappa del valore per regione in", paese, "nel", y),
       fill = "Value")

# Visualizza il plot
print(plot_mappa)




ggplot(data, aes(fill=value)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="GW storage [kg/m^2]") +
  scale_fill_viridis_c(option="viridis", end=0.8)+
  labs(title = paste(paese, y))



