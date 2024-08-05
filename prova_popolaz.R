file_tiff <- "^Data_Raw/population/GHS_POP_E1975_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E1975_GLOBE_R2023A_54009_1000_V1_0.tif"
pop75_t <- raster(file_tiff)
shp <- st_read("^Data/separate/shp/shp.shp")


pop75_t2 <- aggregate(pop75_t, fact=20, fun=sum)
pop75 <- exactextractr::exact_extract(pop75_t2, shp, fun="sum")
pop75 <- data.frame(pop75 = pop75)
pop75$region <- shp$region; pop75$country <- shp$country; pop75$year <- 1975





#########################################
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
  
  # Assegna il dataframe ad una variabile con nome dinamico
  assign(paste0("pop", year), pop_df)
}





