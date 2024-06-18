shp <- st_read("^Data/separate/shp/shp.shp")
pet_t <- raster::brick("^Data_Raw/Global-AI_ET0_v3_annual/et0_v3_yr.tif")
proj4string(pet_t) <- raster::crs(shp)

# Reduce the resolution
factor <- 0.25 / res(pet_t)[1]
pet_t <- aggregate(pet_t, fact=factor, fun=mean, expand=TRUE)

# Merging data
pet <- exactextractr::exact_extract(pet_t, shp, fun="mean")




# Reshape the dataset
pet$region <- shp$region ; pet$country <- shp$country
pet <- reshape2::melt(pet, id.vars=c("country", "region"))

pet_t <- pet_t_aggregated
plot(pet_t)
