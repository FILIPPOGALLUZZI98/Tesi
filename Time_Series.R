# Variazioni temporali delle regioni selezionate di uno stato
# Selezionare lo stato
stato <- "Israel"

state <- subset(shp, CNTRY_NAME == stato)
state$BPL_CODE=NULL; state$CNTRY_CODE=NULL; state$GEOLEVEL1=NULL
gw_data <- exactextractr::exact_extract(r, state, fun="mean")

gw_data$region <- state$ADMIN_NAME
time_ser <- reshape2::melt(gw_data, id.vars="region")
time_ser$date <- 1901 + seq(0, 118)

R <- c(gw_data$region)
# R <- c("", "", "", "", "")
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









