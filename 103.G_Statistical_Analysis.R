suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

shp <- st_read("^Data/shp/shp.shp")
deaths <- read.csv("^Data/Global_deaths.csv")
conflicts <- read.csv("^Data/Global_conflicts.csv")
data_gw <- read.csv("^Data/Global_gws.csv")
data_deaths <-read.csv("^Data/Global_deaths_gws.csv")
data_conflicts <-read.csv("^Data/Global_conflicts_gws.csv")

##############################################################################################################################
####  TEMP  ########################################################################################################################

data <- data_gw_deaths %>%
  group_by(country, region, year, value, type) %>%
  summarise(count = sum(number_deaths))

lm <- lm(log(1+data$count) ~ data$value + as.factor(data$year) + as.factor(data$region))
summary(lm)
plot(data$count ~ data$value )


##############################################################################################################################
####  DEATHS  ########################################################################################################################

# All the data, not divided by type of conflict
fixest::feols(data=data_deaths, log(1+count)~value|region + year)
fixest::feglm(data=data_deaths, count~value|region + year, family=quasipoisson)


fixest::feglm(data=subset(data_deaths, type=="Nstate"), count~value|region + year, family=quasipoisson)

##############################################################################################################################
####  CONFLICTS  ########################################################################################################################

# All the data, not divided by type of conflict
fixest::feols(data=data_conflicts, log(1+conflicts)~value|region + year)
fixest::feglm(data=data_conflicts, log(1+conflicts)~value|region + year, family=quasipoisson)


fixest::feglm(data=subset(data_deaths, type=="Nstate"), count~value|region + year, family=quasipoisson)

















