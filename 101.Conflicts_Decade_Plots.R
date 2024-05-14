events <- st_read("^Data/separate/events_cooridinates/events_coordinates.shp")

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
















