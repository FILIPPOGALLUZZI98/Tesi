# In this code statistical analysis is performed for GWS and Migration data

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer); library(xtable)} )


#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR CONTINENTS (NOT NORMALIZED)

# Upload of the groundwater-migrations dataset
gm <- read.csv("^Data/gws_migr.csv")

# Setting of the dictionary for the tables
setFixest_dict(c(migrants="# migrants/pop", value="gws [g/m^2]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies 1y (1980-2010)", gws_anomalies5="gws anomalies 5y (1980-2010)",
                 gws_anomalies10="gws anomalies 10y (1980-2010)"))


####  GENERALIZED LINEAR REGRESSION FOR CONTINENTS DATA (1-Y)
data_1 <- subset(gm, interval==1)

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Africa)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Asia)

#continent <- "Oceania"
#get_continent <- function(countries) {
#  countrycode(countries, "country.name", "continent")}
#data_continent <- data_1 %>%
#  filter(get_continent(country) == continent)
#Oceania <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
#table<- xtable(etable(Oceania), tex=TRUE)
#print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_migration_oceania_1.tex", include.rownames = FALSE)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Europe)

North_America <- c("United States", "Canada", "Mexico")
N_A <- data_1[data_1$country %in% North_America, ]
Namerica <- fixest::feglm(data=N_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Namerica)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_1[data_1$country %in% South_America, ]
Samerica <- fixest::feglm(data=S_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Samerica)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_1[data_1$country %in% Central_America, ]
Camerica <- fixest::feglm(data=C_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Camerica)



####  GENERALIZED LINEAR REGRESSION FOR CONTINENTS DATA (5-Y)
data_5 <- subset(gm, interval==5)

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Africa)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Asia)

#continent <- "Oceania"
#get_continent <- function(countries) {
#  countrycode(countries, "country.name", "continent")}
#data_continent <- data_5 %>%
#  filter(get_continent(country) == continent)
#Oceania <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
#table<- xtable(etable(Oceania), tex=TRUE)
#print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_migration_oceania_5.tex", include.rownames = FALSE)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Europe)

North_America <- c("United States", "Canada", "Mexico")
N_A <- data_5[data_5$country %in% North_America, ]
Namerica <- fixest::feglm(data=N_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Namerica)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_5[data_5$country %in% South_America, ]
Samerica <- fixest::feglm(data=S_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Samerica)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_5[data_5$country %in% Central_America, ]
Camerica <- fixest::feglm(data=C_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Camerica)



#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR CONTINENTS (NORMALIZED)

# Upload of the groundwater-migrations dataset
gm <- read.csv("^Data/gws_migr_normalized.csv")

# Setting of the dictionary for the tables
setFixest_dict(c(migrants="# migrants/pop", value="gws [g/m^2]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies 1y (1980-2010)", gws_anomalies5="gws anomalies 5y (1980-2010)",
                 gws_anomalies10="gws anomalies 10y (1980-2010)"))


####  GENERALIZED LINEAR REGRESSION FOR CONTINENTS DATA (1-Y)
data_1 <- subset(gm, interval==1)

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Africa)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Asia)

#continent <- "Oceania"
#get_continent <- function(countries) {
#  countrycode(countries, "country.name", "continent")}
#data_continent <- data_1 %>%
#  filter(get_continent(country) == continent)
#Oceania <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
#table<- xtable(etable(Oceania), tex=TRUE)
#print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_migration_oceania_1.tex", include.rownames = FALSE)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Europe)

North_America <- c("United States", "Canada", "Mexico")
N_A <- data_1[data_1$country %in% North_America, ]
Namerica <- fixest::feglm(data=N_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Namerica)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_1[data_1$country %in% South_America, ]
Samerica <- fixest::feglm(data=S_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Samerica)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_1[data_1$country %in% Central_America, ]
Camerica <- fixest::feglm(data=C_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Camerica)



####  GENERALIZED LINEAR REGRESSION FOR CONTINENTS DATA (5-Y)
data_5 <- subset(gm, interval==5)

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Africa)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Asia)

#continent <- "Oceania"
#get_continent <- function(countries) {
#  countrycode(countries, "country.name", "continent")}
#data_continent <- data_5 %>%
#  filter(get_continent(country) == continent)
#Oceania <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
#table<- xtable(etable(Oceania), tex=TRUE)
#print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_migration_oceania_5.tex", include.rownames = FALSE)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data=data_continent, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Europe)

North_America <- c("United States", "Canada", "Mexico")
N_A <- data_5[data_5$country %in% North_America, ]
Namerica <- fixest::feglm(data=N_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Namerica)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_5[data_5$country %in% South_America, ]
Samerica <- fixest::feglm(data=S_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Samerica)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_5[data_5$country %in% Central_America, ]
Camerica <- fixest::feglm(data=C_A, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
etable(Camerica)





















