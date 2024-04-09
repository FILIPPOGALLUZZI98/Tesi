# This is the code to prepare the shapefile and the raster
# Shapefile: remove the non useful variables; remove geometry errors; rename the variables; set the CRS
# Raster: set the CRS, annual mean

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

#################################################################################################
####  SHAPEFILE  ############################################################################

shp <- sf::read_sf("^Data_Raw/world_geolev1_2021/world_geolev1_2021.shp")

# Select the variables of interest
shp$GEOLEVEL1 <- NULL; shp$CNTRY_CODE <- NULL

# Remove regions with geometry error
empty <- st_is_empty(shp); shp <- shp[!empty, ]

# List of empty geometry regions
nomi_geometrie_vuote_rimosse <- rownames(shp)[empty]; print(nomi_geometrie_vuote_rimosse)

# Rename the variables
shp <- shp %>%
  rename(country = CNTRY_NAME,
         region = ADMIN_NAME,
         orig = BPL_CODE)

# Set the country name equal to the region if the country has no subregions
shp$region <- ifelse(is.na(shp$region), shp$country, shp$region)

# Set the CRS
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Save data
st_write(shp, "^Data/shp", driver = "ESRI Shapefile")

#################################################################################################
####  GROUNDWATER STORAGE YEAR AVERAGE  #########################################################

r <- raster::brick("^Data_Raw/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_groundwstor_global_monthly_1901_2019.nc")

# Set the same CRS of the shapefile
proj4string(r) <- raster::crs(shp)

# Annual mean
media_annuale <- lapply(1:119, function(i) {
  anno_iniziale <- (i - 1) * 12 + 1
  anno_finale <- i * 12
  media <- mean(r[[anno_iniziale:anno_finale]])
  return(media)
})

# New rasterbrick with annual averaged values
gws <- brick(media_annuale)

# Save data
years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
names(gws) <- paste0("gws", years)
output_nc <- "^Data/gws.nc"
writeRaster(gws, filename = output_nc, format = "CDF", overwrite = TRUE)

