# In this code statistical analysis is performed for GWS and Conflicts data divided by CONTINENT

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer); library(xtable)} )


#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR CONTINENTS DATA (NOT NORMALIZED)

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")
# ge <- read.csv("^Data/gws_events_normalized.csv")

# Setting of the dictionary for the tables
setFixest_dict(c(conflicts="# conflicts", value="gws [g/m^2]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies 1y (1980-2010)", gws_anomalies5="gws anomalies 5y (1980-2010)",
                 gws_anomalies10="gws anomalies 10y (1980-2010)",count="# conflict"))


# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
events_sum <- subset(ge, type=="state" & year>1988)

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data = data_continent, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
etable(Africa)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data = data_continent, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
etable(Asia)

continent <- "Oceania"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Oceania <- fixest::feglm(data = data_continent, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
etable(Oceania)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data = data_continent, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
etable(Europe)

North_America <- c("United States", "Canada", "Mexico")
N_A <- events_sum[events_sum$country %in% North_America, ]
Namerica <- fixest::feglm(data = N_A, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
etable(Namerica)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- events_sum[events_sum$country %in% South_America, ]
Samerica <- fixest::feglm(data = S_A, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
etable(Samerica)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- events_sum[events_sum$country %in% Central_America, ]
Camerica <- fixest::feglm(data = C_A, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
etable(Camerica)









