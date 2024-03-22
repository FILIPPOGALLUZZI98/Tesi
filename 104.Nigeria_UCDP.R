nig <- read.csv("^Merge_Raster_Shapefile_Gregor/acled_italy.csv")

ggplot(shp) +           
  # plot the outlines of the shapefile of italy (using black borders and transparent filling)
  geom_sf(fill = NA, col = "black") +  
  # plot points for each event, using longitude, latitude as x and y, set shape and color of points using variable "event type" (which is either protests or riots)
  geom_point(data = acled, aes(longitude, latitude, shape = event_type, col = event_type), size=1) + 
  # set nice theme
  theme_bw() +                     
  # separate plots by event type
  facet_wrap(~event_type) +                   
  # hide legend
  theme(legend.position = "none") +          
  # set color scheme
  scale_color_viridis_d(end = 0.7,begin=0.2,option="inferno") +    
  # white background for variable names
  theme(strip.background = element_rect(fill="white"))  +          
  # empty x axis title
  xlab("") +                                         
  # empty y axis title
  ylab("")   

















