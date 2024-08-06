# This code is used to prepare and reshape the raw datasets

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer); library(xtable)} )


#################################################################################################
#################################################################################################
######  INITIAL OPERATIONS FOR THE SHAPEFILE

# Remove undesired variables
shp <- sf::read_sf("^Data_Raw/world_geolev1_2021/world_geolev1_2021.shp")
shp$BPL_CODE=NULL; shp$CNTRY_CODE=NULL

# Remove regions with geometry error and invalid geometries
empty <- st_is_empty(shp); shp <- shp[!empty, ]
shp <- shp[st_is_valid(shp), ]

# List of empty geometry regions
nomi_geometrie_vuote_rimosse <- rownames(shp)[empty]; print(nomi_geometrie_vuote_rimosse)

# Rename the variables country and region
shp <- shp %>%
  rename(country = CNTRY_NAME,
         region = ADMIN_NAME)
# Set the country name equal to the region if the country has no regions
shp$region <- ifelse(is.na(shp$region), shp$country, shp$region)

# Set the CRS
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Save data
st_write(shp, "^Data/separate/shp", driver = "ESRI Shapefile")


#################################################################################################
#################################################################################################
######  INITIAL OPERATIONS FOR THE RASTER OF GWS 

# Open the datasets
r <- raster::brick("^Data_Raw/ISIMIP3a/cwatm_gswp3-w5e5_obsclim_histsoc_default_groundwstor_global_monthly_1901_2019.nc")
shp <- st_read("^Data/separate/shp/shp.shp")

# Set the same CRS of the shapefile
proj4string(r) <- raster::crs(shp)

# Annual mean for all the rasters
media_annuale <- lapply(1:119, function(i) {
  anno_iniziale <- (i - 1) * 12 + 1
  anno_finale <- i * 12
  media <- mean(r[[anno_iniziale:anno_finale]])
  return(media)})

# New rasterbrick with annual averaged values
gws <- brick(media_annuale)

# Save data
years <- unique(format(as.Date(names(r), format = "X%Y.%m.%d"), "%Y"))
names(gws) <- paste0("gws", years)
output_nc <- "^Data/separate/gws.nc"
writeRaster(gws, filename = output_nc, format = "CDF", overwrite = TRUE)


#################################################################################################
#################################################################################################
######  MERGING THE GWS VALUES IN THE REGIONS OF THE SHAPEFILE

# Open shapefile and raster
shp <- st_read("^Data/separate/shp/shp.shp")
r <- raster::brick(paste0("^Data/separate/gws",".nc"))

# Merging data
gw <- exactextractr::exact_extract(r, shp, fun="mean")

# Add columns for regions and countries
gw$region <- shp$region ; gw$country <- shp$country; gw$orig<-shp$GEOLEVEL1

# Reshape the dataset into a long form
gw <- reshape2::melt(gw, id.vars=c("country", "region", "orig"))

# Rename the years
gw$variable <- gsub("mean.X", "", gw$variable)  # Rimuovi "mean.X"
gw$year <- as.integer(gsub("\\D", "", gw$variable)) + 1900 
gw$variable=NULL
gw <- gw[, c("year","country", "region", "value","orig")]

# Since the migration data starts in 1956 (I need 10 years for the averages and the anomalies)
gw <- gw %>%
  filter(year > 1945)

# Rescale GW data (dividing by 1,000)
gw$value <- gw$value/1000

# Save Dataset
write.csv(gw, paste0("^Data/separate/", "gws", ".csv"), row.names=FALSE)
# I obtained a dataset called 'gws' with the value of groundwater storage in each region of the 
# shapefile and for each year


#################################################################################################
#################################################################################################
######  POPULATION DATASET

file_info <- list(
  "1975" = "^Data_Raw/population/GHS_POP_E1975_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E1975_GLOBE_R2023A_54009_1000_V1_0.tif",
  "1980" = "^Data_Raw/population/GHS_POP_E1980_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E1980_GLOBE_R2023A_54009_1000_V1_0.tif",
  "1985" = "^Data_Raw/population/GHS_POP_E1985_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E1985_GLOBE_R2023A_54009_1000_V1_0.tif",
  "1990" = "^Data_Raw/population/GHS_POP_E1990_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E1990_GLOBE_R2023A_54009_1000_V1_0.tif",
  "1995" = "^Data_Raw/population/GHS_POP_E1995_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E1995_GLOBE_R2023A_54009_1000_V1_0.tif",
  "2000" = "^Data_Raw/population/GHS_POP_E2000_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E2000_GLOBE_R2023A_54009_1000_V1_0.tif",
  "2005" = "^Data_Raw/population/GHS_POP_E2005_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E2005_GLOBE_R2023A_54009_1000_V1_0.tif",
  "2010" = "^Data_Raw/population/GHS_POP_E2010_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E2010_GLOBE_R2023A_54009_1000_V1_0.tif",
  "2015" = "^Data_Raw/population/GHS_POP_E2015_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E2015_GLOBE_R2023A_54009_1000_V1_0.tif",
  "2020" = "^Data_Raw/population/GHS_POP_E2020_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E2020_GLOBE_R2023A_54009_1000_V1_0.tif")

shp <- st_read("^Data/separate/shp/shp.shp")

for (year in names(file_info)) {
  file_tiff <- file_info[[year]]
  pop_t <- raster(file_tiff)
  pop_t2 <- aggregate(pop_t, fact=20, fun=sum)
  pop <- exactextractr::exact_extract(pop_t2, shp, fun="sum")
  
  # Crea un dataframe per l'anno corrente
  pop_df <- data.frame(pop = pop)
  pop_df$region <- shp$region
  pop_df$country <- shp$country
  pop_df$year <- as.integer(year)
  assign(paste0("pop", year), pop_df)}

datasets <- list(pop1975, pop1980, pop1985, pop1990, pop1995, pop2000, pop2005, pop2010, pop2015, pop2020)
pop <- bind_rows(datasets)

# Funzione per copiare i valori di pop sugli anni mancanti
add_missing_years <- function(data) {
  years <- seq(1975, 2020, by = 5)
  new_data <- data.frame()
  for (year in years) {
    current_rows <- data %>% filter(year == year)
    for (offset in 1:4) {
      new_rows <- current_rows %>%
        mutate(year = year - offset)
      new_data <- bind_rows(new_data, new_rows)}}
  combined_data <- bind_rows(data, new_data) %>%
    distinct()
  return(combined_data)}

# Applica la funzione a ciascuna combinazione di country e region
pop <- pop %>%
  group_by(country, region) %>%
  do(add_missing_years(.)) %>%
  ungroup()

pop <- pop %>%
  select(year, country, region, pop)

# Save Dataset
write.csv(pop, paste0("^Data/separate/", "pop", ".csv"), row.names=FALSE)


#################################################################################################
#################################################################################################
######  PET DATASET

shp <- st_read("^Data/separate/shp/shp.shp")
pet_t <- raster::brick("^Data_Raw/Global-AI_ET0_v3_annual/et0_v3_yr.tif")
proj4string(pet_t) <- raster::crs(shp)

# Reduce the resolution
factor <- 0.25 / res(pet_t)[1]
pet_t <- aggregate(pet_t, fact=factor, fun=mean, expand=TRUE)

# Merging data
pet <- exactextractr::exact_extract(pet_t, shp, fun="mean")

# Create a new dataset
country <- shp$country
pet <- data.frame(country=country,pet = pet)

# Media per ogni country
pet <- pet %>%
  group_by(country) %>%
  summarize(pet = mean(pet))

# Ordinare il dataset in base al valore di PET
pet <- pet %>%
  arrange(pet)

# Save data
write.csv(pet, paste0("^Data/separate/", "pet", ".csv"), row.names=FALSE)








