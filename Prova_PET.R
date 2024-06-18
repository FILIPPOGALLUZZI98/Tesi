pet_t <- raster::brick("^Data_Raw/")
shp <- st_read("^Data/separate/shp/shp.shp")

# Set the same CRS of the shapefile
proj4string(pet) <- raster::crs(shp)

# Merging data
pet <- exactextractr::exact_extract(pet_t, shp, fun="mean")

# Add columns for regions and countries
pet$region <- shp$region ; pet$country <- shp$country; pet$orig<-shp$GEOLEVEL1

# Reshape the dataset into a long form
pet <- reshape2::melt(pet, id.vars=c("country", "region", "orig"))

# Rename the years
pet$variable <- gsub("mean.X", "", gw$variable)  # Rimuovi "mean.X"
pet$year <- as.integer(gsub("\\D", "", pet$variable)) + 1900 
pet$variable=NULL
pet <- pet[, c("year","country", "region", "value","orig")]

