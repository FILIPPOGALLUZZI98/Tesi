file_path <- "^Data_Raw/Conflict_Data/Global.csv"
events <- read.csv(file_path)

# Rename the variables
events <- events %>%
  rename(type = type_of_violence,
         number_deaths = best)
events <- mutate(events,
                 type = case_when(
                   type == 1 ~ "state",
                   type == 2 ~ "Nstate",
                   type == 3 ~ "onesided"
                 ))

# Set the coordinate system
events <- st_as_sf(events, coords = c("longitude", "latitude"), crs = st_crs(shp))
events <- st_transform(events, st_crs(shp))

# Remove the invalid geometries from shp
shp <- shp[st_is_valid(shp), ]


events1 <- subset(events, year >= 1989 & year <= 1999)
events2 <- subset(events, year >= 2000 & year <= 2009)
events3 <- subset(events, year >= 2010 & year <= 2019)

plot_final <- ggplot() +
  geom_sf(data = shp) +
  geom_sf(data = events1, color = "red", size=0.5)
print(plot_final)

plot_final <- ggplot() +
  geom_sf(data = shp) +
  geom_sf(data = events2, color = "red", size=0.5)
print(plot_final)

plot_final <- ggplot() +
  geom_sf(data = shp) +
  geom_sf(data = events3, color = "red", size=0.5)
print(plot_final)
















