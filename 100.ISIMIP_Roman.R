library(sf)              # useful for spatial manipulations
library(sp)              # useful for spatial manipulations
library(raster)          # useful for working with raster data
library(ncdf4)           # useful for working with raster data
library(exactextractr)   # useful for extracting data from raster files
library(dplyr)           # useful for merging data sets
library(reshape2)        # useful for manipulating data sets
library(ggplot2)         # useful for data visualization
library(ggrepel)         # useful for labeling point plots in ggplot2
library(zoo)   


shp = sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp = subset(shp, CNTRY_NAME == "Sudan")
plot(shp[,"geometry"])  
shp = sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 

r <- raster::brick("GW_Data/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_groundwstor_global_monthly_1901_2019.nc")
proj4string(r) <- raster::crs(shp)    
image(r[[1]]) 

names(r) <- paste0(rep(paste0(1901:2018), each=12), "-", stringr::str_pad(rep(1:12, length(1901:2019)), 2, "left", "0"))
plot.df <- as.data.frame(r[[12]], xy = TRUE)  ## 12 equivale a dicembre 1901
plot.df <- plot.df[complete.cases(plot.df), ]


gw_data <- exactextractr::exact_extract(r, shp, fun="mean")
shp$gw_1902_12 <- gw_data$mean.X1901.12.2

ggplot(shp, aes(fill=gw_1902_12)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="inferno", end=0.8)










