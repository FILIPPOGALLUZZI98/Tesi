file_tiff <- "^Data_Raw/population/GHS_POP_E1975_GLOBE_R2023A_54009_1000_V1_0/GHS_POP_E1975_GLOBE_R2023A_54009_1000_V1_0.tif"
pop_t <- raster(file_tiff)
shp <- st_read("^Data/separate/shp/shp.shp")


factor <- 25
pop_t2 <- aggregate(pop_t, fact=factor, fun=sum)

proj4string(pop) <- raster::crs(shp)
pop <- exactextractr::exact_extract(pop_t2, shp, fun="sum")
pop <- data.frame(pop = pop)
pop$region <- shp$region; pop$country <- shp$country 
pop$year <- 1975






