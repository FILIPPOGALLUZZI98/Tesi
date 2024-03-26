# Scegliere quale raster usare (rs, rt, rq)
r <- rs
# Selezionare lo stato
stato <- "Israel"




state <- subset(shp, CNTRY_NAME == stato)
gw_data_t <- exactextractr::exact_extract(r, state, fun="mean")
gw_data_t$region <- state$ADMIN_NAME

time_ser <- reshape2::melt(gw_data_t, id.vars="region")
time_ser$date <- 1901 + seq(0, 118)

R <- c(gw_data_t$region)
# R <- c("", "", "", "", "")
ggplot(subset(time_ser, region %in% R), 
       aes(date, value, fill=value, col=value)) +   
  geom_hline(yintercept=0) +                  
  geom_hline(yintercept=-1.5, lty=3) +     
  facet_wrap(region~., ncol=2) +        
  theme_bw() +         
  theme(strip.background=element_rect(fill="white")) +          
  ylab("SPEI-12") +                                                                
  xlab("") +                                                                       
  geom_col() +                                                                      
  scale_fill_viridis_c(option="inferno", end  = 0.8) +                              
  scale_color_viridis_c(option="inferno", end = 0.8)









