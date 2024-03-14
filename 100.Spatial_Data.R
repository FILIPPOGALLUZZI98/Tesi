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




# Drought in Italy
r <- raster::brick("spatialRlab/spei12_italy.nc")    
# set a coordinate system for the raster data (the same as in the shapefile, because we want to merge them later)
proj4string(r) = raster::crs(shp)         
# basic and quick visualization of the first raster layer
image(r[[1]]) 

# First, we give names to each of the raster layers, based on the corresponding year and months. Second, we build 
# a data.frame from the raster data
names(r)       = paste0(rep(paste0(1982:2022), each=12), "-", stringr::str_pad(rep(1:12, length(1982:2022)), 2, "left", "0"))
# this creates a data frame from the raster layer 488 (= august 2022)
plot.df        = as.data.frame(r[[488]], xy = TRUE) 
# delete missing observations (mostly the mediterranean sea)
plot.df        = plot.df[complete.cases(plot.df), ] 

# Plot
ggplot() +     
  # plot the shape file as background layer (with grey borders and white provinces)
  geom_sf(data=shp, col="grey", fill="white") +  
  # geom_tile() is used to plot raster data here, with the filling color for each tile based on variable "X2022.08"
  geom_tile(data=plot.df, aes(x,y,fill=X2022.08)) + 
   # use a nicer theme than standard
  theme_bw() +                           
  # title for the legend
  labs(fill="SPEI-12\nin Aug. 2022") +          
  # again, we use the viridis color scheme
  scale_fill_viridis_c(option="inferno", end=0.8) +
  # no title on x axis
  xlab("") +               
  # no title on y axis
  ylab("")          




# AGGREGATING RASTER DATA USING POLYGON DATA
spei_data        = exactextractr::exact_extract(r, shp, fun="mean")

# create a column that holds the names of the provinces
spei_data$region = shp$NUTS_NAME

# now let's add the extracted data from august 2022 back to the shape file so we can visualize it using the polygon data
shp$spei_2022_08 = spei_data$mean.X2022.08

ggplot(shp, aes(fill=spei_2022_08)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="Avg. SPEI-12\nin Aug. 2022") +
  scale_fill_viridis_c(option="inferno", end=0.8)




# We can also visualize the SPEI-12 over time for each region
plot.df      = reshape2::melt(spei_data, id.vars="region")

# This line of code uses the "zoo" package and creates a variable of class "date".
# This will be useful for plotting the SPEI-12 index across time, because ggplot2 naturally handles "date" variables on the x-axis.
plot.df$date = zoo::as.Date(zoo::as.yearmon(substr(as.character(plot.df$variable), 7, 13), "%Y.%m"))

# subset data to some provinces
ggplot(subset(plot.df, region %in% c("Pordenone", "Milano", "Palermo", "Bologna")), 
       # date on x axis, spei (stored in variable "value") on y axis, color (of outlines) based on spei, filling based on spei
       aes(date, value, fill=value, col=value)) +   
  # add a horizontal line at zero
  geom_hline(yintercept=0) +                  
  # add a horizontal, dotted (lty=3) line at -1.5 to visualize the threshold for severe drought
  geom_hline(yintercept=-1.5, lty=3) +     
  # split the plot into separate plots for each region, organized in two columns
  facet_wrap(region~., ncol=2) +        
  # nicer theme than standard theme
  theme_bw() +         
  # white background for province names
  theme(strip.background=element_rect(fill="white")) +          
  # y axis title
  ylab("SPEI-12") +                                                                
  # x axis title
  xlab("") +                                                                       
  # columns for SPEI in each month
  geom_col() +                                                                      
  # set color palette (for the outlines of the columns)
  scale_fill_viridis_c(option="inferno", end  = 0.8) +                              
  # set fill palette (for filling the columns)
  scale_color_viridis_c(option="inferno", end = 0.8) +                              
  # set legend title
  labs(fill="SPEI-12",col="SPEI-12")   




# RIOTS











