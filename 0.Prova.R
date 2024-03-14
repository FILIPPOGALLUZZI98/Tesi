setwd("D:Personale/Studio/^Magistrale_Tesi/zDatasets")

suppressPackageStartupMessages({
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
})

shp = sf::read_sf("spatialRlab/nuts/NUTS_RG_60M_2021_3035.shp") # read the shapefile (note the "geometry" variable)
plot(shp[,"geometry"])  

shp = subset(shp, CNTR_CODE == "TR")                # subset to regions located in Turkey
shp = subset(shp, LEVL_CODE == 3)                   # subset to NUTS3 level (=province level)
plot(shp[,"geometry"]) 
shp = sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 




r <- raster::brick("ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_groundwstor_global_monthly_1901_2019.nc")    
# set a coordinate system for the raster data (the same as in the shapefile, because we want to merge them later)
proj4string(r) = raster::crs(shp)         
# basic and quick visualization of the first raster layer
image(r[[1]]) 


names(r)       = paste0(rep(paste0(1901:2019), each=12), "-", stringr::str_pad(rep(1:12, length(1901:2019)), 2, "left", "0"))
plot.df        = as.data.frame(r[[1]], xy = TRUE) 
plot.df        = plot.df[complete.cases(plot.df), ] 

gwstr_data        = exactextractr::exact_extract(r, shp, fun="mean")
gwstr_data$region = shp$NUTS_NAME



shp$gwstr_2000_01 = gwstr_data$mean.X2000.01

ggplot(shp, aes(fill=gwstr_2000_01)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="Avg. gwstr 01/01/2000") +
  scale_fill_viridis_c(option="viridis", end=0.8)


















