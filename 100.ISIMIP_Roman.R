suppressPackageStartupMessages({
  library(sf)              ## useful for spatial manipulations
  library(sp)              ## useful for spatial manipulations
  library(raster)          ## useful for working with raster data
  library(ncdf4)           ## useful for working with raster data
  library(exactextractr)   ## useful for extracting data from raster files
  library(dplyr)           ## useful for merging data sets
  library(reshape2)        ## useful for manipulating data sets
  library(ggplot2)         ## useful for data visualization
  library(ggrepel)         ## useful for labeling point plots in ggplot2
  library(lubridate)
  library(zoo)   
})

#################################################################################################
####  GW DATA MEDIE ANNUALI  ####################################################################
###    Medie annuali dei dati raster ISIMIP3a del groundwstr a partire dai dati mensili
###    r <- raster::brick("GW_Data/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_groundwstor_global_monthly_1901_2019.nc")
###    Creazione di un nuovo raster brick per le medie annuali
###    gwy <- raster::brick(ncol = ncol(r), nrow = nrow(r), nl = nlayers(r)/12,
###                         xmn = extent(r)[1], xmx = extent(r)[2], 
###                         ymn = extent(r)[3], ymx = extent(r)[4], 
###                         crs = crs(r))
###    Ciclo for per calcolare le medie annuali
###    for (i in 1:(nlayers(r)/12)) {
###      start <- (i - 1) * 12 + 1
###      end <- start + 11
###      yearly_mean <- calc(r[[start:end]], mean)
###      gwy[[i]] <- yearly_mean}
###    years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
###    names(gwy) <- paste0("gw", years)
###    output_nc <- "GW_Data/ISIMIP3a/gwy.nc"
###    Scrivi il RasterBrick in un file NetCDF
###    writeRaster(gwy, filename = output_nc, format = "CDF", overwrite = TRUE)


#################################################################################################
####  CARICARE I DATI  #########################################################################
# Upload dei dati shapefile e gw medie annuali + coordinate uguali
shp <- sf::read_sf("GW_Data/world_geolev1_2021/world_geolev1_2021.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
r <- raster::brick("GW_Data/ISIMIP3a/gwy.nc")
proj4string(r) <- raster::crs(shp)


#################################################################################################
####  PLOT FOR SINGLE STATE  ####################################################################
# Per ogni stato faccio i grafici di groundwstr nelle regioni per ogni anno
# Seleziono il paese
state <- subset(shp, CNTRY_NAME == "Italy")
# Media dei valori del raster sulle regioni 
gw_data <- exactextractr::exact_extract(r, state, fun="mean")
gw_data$region <- state$ADMIN_NAME
# Rinomino gli anni
for (year in 1901:2019) {
  col_name <- paste0("gw_", year)
  state[[col_name]] <- gw_data[[paste0("mean.X", year - 1900)]]
}

# Plot anno 2000
ggplot(state, aes(fill=gw_2000)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)


################################################################################################
####  ESEMPIO PER POCHI STATI PER VEDERE SE FUNZIONA  ##########################################
gw_data <- list()
state <- list()
nomi <- c("Italy","Austria","Germany")

for (i in seq_along(nomi)) {
  a <- subset(shp, CNTRY_NAME == nomi[i])
  state <- append(state, list(a))
}

for (i in 1:3) {
    b <- exactextractr::exact_extract(r, state[[i]], fun="mean")
    b$region <- state[[i]]$ADMIN_NAME
    gw_data <- append(gw_data, list(b))
}

# Plot anno 2000 per Italia
ggplot(state[[1]], aes(fill=gw_data[[1]]$mean.X117)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)

################################################################################################
################################################################################################
gw_data <- list()
state <- list()
nomi <- unique(shp$CNTRY_NAME)
posizioni_da_rimuovere <- c(10, 63, 66, 72, 74, 99, 108, 122, 134, 145, 161, 163, 
                            175, 207, 209, 211, 233, 242, 245)
nomi_rimossi <- nomi[posizioni_da_rimuovere]
nomi <- nomi[-posizioni_da_rimuovere]
print(nomi_rimossi)

for (i in seq_along(nomi)) {
  a <- subset(shp, CNTRY_NAME == nomi[i])
  state <- append(state, list(a))
}

for (i in 1:264) {
    b <- exactextractr::exact_extract(r, state[[i]], fun="mean")
    b$region <- state[[i]]$ADMIN_NAME
    gw_data <- append(gw_data, list(b))
}

# Plot anno 2000 per Iran


ggplot(state[[99]], aes(fill=gw_data[[99]]$mean.X117)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)



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







