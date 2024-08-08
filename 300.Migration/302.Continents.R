# In this code statistical analysis is performed for GWS and Migration data divided by CONTINENTS
# Generalized linear regression with fixed effects (region and year)
# Family: gaussian

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest);library(xtable); library(data.table)} )


#################################################################################################
#################################################################################################

# Upload of the groundwater-migrations dataset
gm <- read.csv("^Data/gws_migr.csv")

# Setting of the dictionary for the tables
setFixest_dict(c(migrants="# migrants/pop", value="gws [Kg/m^2]",
                 gws_avg1="average 1-y", gws_avg5="average 5-y", gws_avg10="average 10-y",
                 gws_growth1="growth rate (%) 1-y", gws_growth5="growth rate (%) 5-y", gws_growth10="growth rate (%) 10-y",
                 gws_std1="STD 1-y", gws_std5="STD 5-y", gws_std10="STD 10-y",
                 gws_anomalies="anomalies 1y", gws_anomalies5="gws anomalies 5y",
                 gws_anomalies10="gws anomalies 10y", n_value="normalized gws",
                 n_gws_avg1="normalized average 1-y", n_gws_avg5="normalized average 5-y",n_gws_avg10="normalized average 10-y",
                 CV1="Coefficient of variation 1-y", CV5="Coefficient of variation 5-y",CV10="Coefficient of variation 10-y"))



####  ANALYSIS FOR 1-Y INTERVAL
data_1 <- subset(gm, interval==1)

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Africa); write.csv(tabella, "^Tabelle/migration_Africa_1.csv", row.names = FALSE)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Asia); write.csv(tabella, "^Tabelle/migration_Asia_1.csv", row.names = FALSE)

continent <- "Oceania"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Oceania <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Oceania); write.csv(tabella, "^Tabelle/migration_Oceania_1.csv", row.names = FALSE)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Europe); write.csv(tabella, "^Tabelle/migration_Europe_1.csv", row.names = FALSE)

North_America <- c("United States", "Canada", "Mexico")
N_A <- data_1[data_1$country %in% North_America, ]
Namerica <- fixest::feglm(data=N_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Namerica); write.csv(tabella, "^Tabelle/migration_Namerica_1.csv", row.names = FALSE)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_1[data_1$country %in% South_America, ]
Samerica <- fixest::feglm(data=S_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Samerica); write.csv(tabella, "^Tabelle/migration_Samerica_1.csv", row.names = FALSE)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_1[data_1$country %in% Central_America, ]
Camerica <- fixest::feglm(data=C_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Camerica); write.csv(tabella, "^Tabelle/migration_Camerica_1.csv", row.names = FALSE)



####  GENERALIZED LINEAR REGRESSION FOR CONTINENTS DATA (5-Y)
data_5 <- subset(gm, interval==5)

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Africa); write.csv(tabella, "^Tabelle/migration_Africa_5.csv", row.names = FALSE)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Asia); write.csv(tabella, "^Tabelle/migration_Asia_5.csv", row.names = FALSE)

continent <- "Oceania"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Oceania <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Oceania); write.csv(tabella, "^Tabelle/migration_Oceania_5.csv", row.names = FALSE)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Europe); write.csv(tabella, "^Tabelle/migration_Europe_5.csv", row.names = FALSE)

North_America <- c("United States", "Canada", "Mexico")
N_A <- data_5[data_5$country %in% North_America, ]
Namerica <- fixest::feglm(data=N_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Namerica); write.csv(tabella, "^Tabelle/migration_Namerica_5.csv", row.names = FALSE)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_5[data_5$country %in% South_America, ]
Samerica <- fixest::feglm(data=S_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Samerica); write.csv(tabella, "^Tabelle/migration_Samerica_5.csv", row.names = FALSE)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_5[data_5$country %in% Central_America, ]
Camerica <- fixest::feglm(data=C_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=gaussian)
tabella <- etable(Camerica); write.csv(tabella, "^Tabelle/migration_Camerica_5.csv", row.names = FALSE)













