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

# Selezionare lo stato
stato <- "Israel"

state <- subset(shp, CNTRY_NAME == stato)
gw_data <- exactextractr::exact_extract(r, state, fun="mean")

gw_data$region <- state$ADMIN_NAME
time_ser <- reshape2::melt(gw_data, id.vars="region")
time_ser$date <- 1901 + seq(0, 118)

R <- c(gw_data$region)
ggplot(subset(time_ser, region %in% R), 
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
  scale_color_viridis_c(option="inferno", end = 0.8)









