suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

shp <- st_read("^Data/shp/shp.shp")
deaths <- read.csv("^Data/Global_deaths.csv")
conflicts <- read.csv("^Data/Global_conflicts.csv")
data_gw <- read.csv("^Data/Global_gws.csv")
data_gw_deaths <-read.csv("^Data/Global_deaths_gws.csv")
data_gw_conflicts <-read.csv("^Data/Global_conflicts_gws.csv")


data <- data_gw_deaths %>%
  group_by(country, region, year, value, type) %>%
  summarise(count = sum(number_deaths))

lm <- lm(log(1+data$count) ~ data$value + as.factor(data$year) + as.factor(data$region))
summary(lm)
plot(data$count ~ data$value )



fixest::feols(data=data, log(1+count)~value|region + year)

fixest::feglm(data=data, count~value|region + year, family=quasipoisson)

fixest::feglm(data=subset(data, type=="Nstate"), count~value|region + year, family=quasipoisson)

              
