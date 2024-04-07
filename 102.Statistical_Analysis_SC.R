# Select the country
paese <- "Nigeria"
# Select the raster
rast <- "gws"


suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

shp <- st_read("^Data/shp/shp.shp")
state <- subset(shp, country == paese)  ## plot(state[,"geometry"])

path <- paste0("^Data/Single_Country/", paese, "/")
events <-read.csv(paste0(path, paese, "_events.csv"))
data_gw <- read.csv(paste0(path, paese, "_",rast, ".csv"))
data_gw_events <- read.csv(paste0(path, paese, "_", rast,"_events",".csv"))

######################################################################################################
data <- data_gw_events %>%
  group_by(year, region, value) %>%
  summarise(number = sum(number_deaths))

lm <- lm(data$number ~ data$value + as.factor(data$year) + as.factor(data$region))
summary(lm)
plot(lm)

plot(data$number ~ data$value )
abline(lm) # Per fare la retta di regressione sul grafico dei punti



















