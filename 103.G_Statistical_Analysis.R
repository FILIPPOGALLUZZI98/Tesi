suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode)})

# shp <- st_read("^Data/shp/shp.shp")
# deaths <- read.csv("^Data/Global_deaths.csv")
# conflicts <- read.csv("^Data/Global_conflicts.csv")
# data_gw <- read.csv("^Data/Global_gws.csv")
data_deaths <-read.csv("^Data/Global_deaths_gws.csv")
data_conflicts <-read.csv("^Data/Global_conflicts_gws.csv")


##############################################################################################################################
####  DEATHS  ########################################################################################################################

lm <- lm(log(1+data_deaths$deaths) ~ data_deaths$value + as.factor(data_deaths$year) + as.factor(data_deaths$region))
summary(lm)
plot(data_deaths$deaths ~ data_deaths$value)

# All the data, not divided by type of conflict
fixest::feols(data=data_deaths, log(1+deaths)~value|region + year)
fixest::feglm(data=data_deaths, log(1+deaths)~value|region + year, family=quasipoisson)

# For the type of conflict
fixest::feglm(data=subset(data_deaths, type=="Nstate"), log(1+deaths)~value|region + year, family=quasipoisson)

# For the continent
continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")
}
conflict_continent <- data_deaths %>%
  filter(get_continent(country) == continent)
fixest::feglm(data=conflict_continent, log(1+deaths)~value|region + year, family=quasipoisson)

##############################################################################################################################
####  CONFLICTS  ########################################################################################################################

# All the data, not divided by type of conflict
fixest::feols(data=data_conflicts, log(1+conflicts)~value|region + year)
fixest::feglm(data=data_conflicts, conflicts~value|region + year, family=quasipoisson)

# For the type of conflict
fixest::feglm(data=subset(data_conflicts, type=="Nstate"), conflicts~value|region + year, family=quasipoisson)

# For the continent
continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")
}
conflict_continent <- data_conflicts %>%
  filter(get_continent(country) == continent)

fixest::feols(data=conflict_continent, log(1+conflicts)~value|region + year)
fixest::feglm(data=conflict_continent, conflicts~value|region + year, family=quasipoisson)













