# In this code statistical analysis is performed for GWS and Migration data divided by CONTINENTS
# Generalized linear regression with fixed effects (region and year)
# Family: gaussian

#################################################################################################
#################################################################################################

# Upload of the groundwater-migrations dataset
gm <- read.csv("^Data/gws_migr.csv")
data_1 <- subset(gm, interval==1)
data_5 <- subset(gm, interval==5)

####  AFRICA
continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Africa1 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Africa1)
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Africa5 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Africa5) 
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_Africa.csv", row.names = FALSE)


####  ASIA
continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Asia1 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Asia1)
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Asia5 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Asia5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_Asia.csv", row.names = FALSE)


#### OCEANIA
continent <- "Oceania"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Oceania <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Oceania1)
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Oceania5 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Oceania5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_Oceania.csv", row.names = FALSE)


## EUROPE
continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Europe1 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Europe1)
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Europe5 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Europe5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_Europe.csv", row.names = FALSE)


####  NORTH AMERICA
North_America <- c("United States", "Canada", "Mexico")
N_A <- data_1[data_1$country %in% North_America, ]
Namerica1 <- fixest::feglm(data=N_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Namerica1)
N_A <- data_5[data_5$country %in% North_America, ]
Namerica5 <- fixest::feglm(data=N_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Namerica5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_Namerica.csv", row.names = FALSE)


#### SOUTH AMERICA
South_America <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_1[data_1$country %in% South_America, ]
Samerica1 <- fixest::feglm(data=S_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Samerica1)
S_A <- data_5[data_5$country %in% South_America, ]
Samerica5 <- fixest::feglm(data=S_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Samerica5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_Samerica.csv", row.names = FALSE)


#### CENTRAL AMERICA
Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Jamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_1[data_1$country %in% Central_America, ]
Camerica1 <- fixest::feglm(data=C_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Camerica1)
C_A <- data_5[data_5$country %in% Central_America, ]
Camerica5 <- fixest::feglm(data=C_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Camerica5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_Camerica.csv", row.names = FALSE)













