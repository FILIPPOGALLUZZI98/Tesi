#################################################################################################
####  PLOT TIME SERIES  ##########################################################################
plot.df <- as.data.frame(r[[119]], xy = TRUE)  ## 119 equivale a dicembre 2019
plot.df <- plot.df[complete.cases(plot.df), ]
plot.df <- reshape2::melt(gw_data, id.vars="region")
plot.df$date <- zoo::as.Date(zoo::as.yearmon(substr(as.character(plot.df$variable), 7, 13), "%Y.%m"))


# Plottiamo le serie temporali per 4 regioni del Sudan
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
  scale_color_viridis_c(option="inferno", end = 0.8)







