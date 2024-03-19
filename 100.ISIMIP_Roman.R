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
gw_data$region = shp$ADMIN_NAME
shp$gw_1902_12 <- gw_data$mean.X1901.12.2

ggplot(shp, aes(fill=gw_1902_12)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="inferno", end=0.8)




# PLOT DIFFERENT REGIONS
plot.df <- reshape2::melt(gw_data, id.vars="region")
plot.df$date <- zoo::as.Date(zoo::as.yearmon(substr(as.character(plot.df$variable), 7, 13), "%Y.%m"))
ggplot(subset(plot.df, region %in% c("Red Sea", "Kassala", "Northern", "Al Gezira")), 
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
  ylab("GW Storage") +                                                                
  # x axis title
  xlab("") +                                                                       
  # columns for GW Storage in each month
  geom_col() +                                                                      
  # set color palette (for the outlines of the columns)
  scale_fill_viridis_c(option="inferno", end  = 0.8) +                              
  # set fill palette (for filling the columns)
  scale_color_viridis_c(option="inferno", end = 0.8) +                              
  # set legend title
  labs(fill="SPEI-12",col="GW Storage") 







