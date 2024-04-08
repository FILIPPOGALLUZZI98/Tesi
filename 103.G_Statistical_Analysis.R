suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode)})

data_events <-read.csv("^Data/Global_gws_events.csv")

##############################################################################################################################
####  CONFLICTS  ########################################################################################################################

# All the data, not divided by type of conflict
fixest::feols(data=data_events, log(1+conflicts)~value|region + year)
fixest::feglm(data=data_events, conflicts~value|region + year, family=quasipoisson)

# For the type of conflict
fixest::feglm(data=subset(data_events, type=="Nstate"), conflicts~value|region + year, family=quasipoisson)

# For the continent
continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")
}
conflict_continent <- data_events %>%
  filter(get_continent(country) == continent)

fixest::feols(data=conflict_continent, log(1+conflicts)~value|region + year)
fixest::feglm(data=conflict_continent, conflicts~value|region + year, family=quasipoisson)




##############################################################################################################################
####  DEATHS  ########################################################################################################################

lm <- lm(log(1+data_events$number_deaths) ~ data_events$value + as.factor(data_events$year) + as.factor(data_events$region))
summary(lm)
plot(data_events$number_deaths ~ data_events$value)

# All the data, not divided by type of conflict
fixest::feols(data=data_events, log(1+number_deaths)~value|region + year)
fixest::feglm(data=data_events, log(1+number_deaths)~value|region + year, family=quasipoisson)

# For the type of conflict
fixest::feglm(data=subset(data_events, type=="Nstate"), log(1+number_deaths)~value|region + year, family=quasipoisson)

# For the continent
continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")
}
conflict_continent <- data_events %>%
  filter(get_continent(country) == continent)
fixest::feglm(data=conflict_continent, log(1+number_deaths)~value|region + year, family=quasipoisson)












