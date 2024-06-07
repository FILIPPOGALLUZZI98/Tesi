# In this code statistical analysis is performed for GWS and Conflicts data divided by CONTINENT
# Generalized linear regression with fixed effects (region and year)
# Family: quasipoisson

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest);library(xtable); library(data.table)} )


#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")

# Setting of the dictionary for the tables
setFixest_dict(c(conflicts="# conflicts", value="gws [Kg/m^2]",
                 gws_avg1="average 1-y", gws_avg5="average 5-y", gws_avg10="average 10-y",
                 gws_growth1="growth rate (%) 1-y", gws_growth5="growth rate (%) 5-y", gws_growth10="growth rate (%) 10-y",
                 gws_std1="STD 1-y", gws_std5="STD 5-y", gws_std10="STD 10-y",
                 gws_anomalies="anomalies 1y", gws_anomalies5="gws anomalies 5y",
                 gws_anomalies10="gws anomalies 10y",count="# conflicts", 
                 n_confl="normalized conflicts", n_count="normalized conflicts", n_value="normalized gws",
                 n_gws_avg1="normalized average 1-y", n_gws_avg5="normalized average 5-y,n_gws_avg10="normalized average 10-y,
                 CV1="Coefficient of variation 1-y", CV5="Coefficient of variation 5-y",CV10="Coefficient of variation 10-y"))


# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
events_sum <- subset(ge, type=="state")


# Statistical model and tables

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data = data_continent, count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
n_Africa <- fixest::feglm(data = data_continent, n_count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(Africa); df <- as.data.frame(tabella)
write.csv(tabella, "^Tabelle/conflicts_Africa.csv", row.names = FALSE)
n_tabella <- etable(n_Africa); df <- as.data.frame(n_tabella)
write.csv(n_tabella, "^Tabelle/conflicts_Africa_n.csv", row.names = FALSE)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data = data_continent, count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
n_Asia <- fixest::feglm(data = data_continent, n_count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(Asia); df <- as.data.frame(tabella)
write.csv(tabella, "^Tabelle/conflicts_Asia", row.names = FALSE)
n_tabella <- etable(n_Asia); df <- as.data.frame(n_tabella)
write.csv(n_Asia, "^Tabelle/conflicts_Asia_n.csv", row.names = FALSE)

continent <- "Oceania"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Oceania <- fixest::feglm(data = data_continent, count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
n_Oceania <- fixest::feglm(data = data_continent, n_count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(Oceania); df <- as.data.frame(tabella)
write.csv(tabella, "^Tabelle/conflicts_Oceania.csv", row.names = FALSE)
n_tabella <- etable(n_Oceania); df <- as.data.frame(n_tabella)
write.csv(n_tabella, "^Tabelle/conflicts_Oceania_n.csv", row.names = FALSE)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data = data_continent, count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
n_Europe <- fixest::feglm(data = data_continent, n_count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(Europe); df <- as.data.frame(tabella)
write.csv(tabella, "^Tabelle/conflicts_Europe.csv", row.names = FALSE)
n_tabella <- etable(n_Europe); df <- as.data.frame(n_tabella)
write.csv(n_tabella, "^Tabelle/conflicts_Europe_n.csv", row.names = FALSE)

North_America <- c("United States", "Canada", "Mexico")
N_A <- events_sum[events_sum$country %in% North_America, ]
Namerica <- fixest::feglm(data = N_A, count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
n_Namerica <- fixest::feglm(data = N_A, n_count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(Namerica); df <- as.data.frame(tabella)
write.csv(tabella, "^Tabelle/conflicts_Namerica.csv", row.names = FALSE)
n_tabella <- etable(n_Namerica); df <- as.data.frame(n_tabella)
write.csv(n_tabella, "^Tabelle/conflicts_Namerica_n.csv", row.names = FALSE)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- events_sum[events_sum$country %in% South_America, ]
Samerica <- fixest::feglm(data = S_A, count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
n_Samerica <- fixest::feglm(data = S_A, n_count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(Samerica); df <- as.data.frame(tabella)
write.csv(tabella, "^Tabelle/conflicts_Samerica.csv", row.names = FALSE)
n_tabella <- etable(n_Samerica); df <- as.data.frame(n_tabella)
write.csv(n_tabella, "^Tabelle/conflicts_Samerica_n.csv", row.names = FALSE)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- events_sum[events_sum$country %in% Central_America, ]
Camerica <- fixest::feglm(data = C_A, count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
n_Camerica <- fixest::feglm(data = C_A, n_count~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(Camerica); df <- as.data.frame(tabella)
write.csv(tabella, "^Tabelle/conflicts_Camerica.csv", row.names = FALSE)
n_tabella <- etable(n_Camerica); df <- as.data.frame(n_tabella)
write.csv(n_tabella, "^Tabelle/conflicts_Camerica_n.csv", row.names = FALSE)









