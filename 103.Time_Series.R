stato <- "Italy"
state <- subset(shp, CNTRY_NAME == stato)
gw_data <- exactextractr::exact_extract(r, state, fun="mean")

gw_data$region <- state$ADMIN_NAME
plot.df <- reshape2::melt(gw_data, id.vars="region")

R <- c(gw_data$region)
ggplot(subset(plot.df, region %in% R), 
       # date on x axis, spei (stored in variable "value") on y axis, color (of outlines) based on spei, filling based on spei
       aes(variable, value, fill=value, col=value)) +   
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









