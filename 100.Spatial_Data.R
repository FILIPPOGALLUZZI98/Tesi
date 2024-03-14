install.packages(c("sf", "sp", "raster", "ncdf4", "exactextractr", "dplyr", "reshape2", "ggplot2", "ggrepel", "zoo"))

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




# Import shapefile NUTS3 regions in Europe
shp = sf::read_sf("spatialRlab/nuts/NUTS_RG_60M_2021_3035.shp") # read the shapefile (note the "geometry" variable)
plot(shp[,"geometry"])  

# To select just Italy
shp = subset(shp, CNTR_CODE == "IT")                # subset to regions located in italy
shp = subset(shp, LEVL_CODE == 3)                   # subset to NUTS3 level (=province level)
plot(shp[,"geometry"])                              

# Set the appropriate reference system
shp = sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 




# MERGE POPULATION DATA TO OUR SHAPEFILE
popdat = read.csv("spatialRlab/italy_population_nuts3.csv")
shp = left_join(shp, popdat, by=c("NUTS_ID" = "nuts3_it"))

# Plottiamo la distribuzione di popolazione dell'Italia nel 2022 usando 'ggplot2' 
ggplot(shp, aes(fill = log(population2022))) +      
  # geom_sf() plots polygon data from an sf object (with black outlines)
  geom_sf(col="black") +                   
  # set a theme that is more visually appealing than the standard theme of ggplo2
  theme_bw() +         
  # set viridis color palette for filling, with some customization options
  scale_fill_viridis_c(option="inferno", end=0.8) +     
  # rename the legend title
  labs(fill = "Log(Population 2022)")    











