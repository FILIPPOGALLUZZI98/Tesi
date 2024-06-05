# In this code statistical analysis is performed for Conflicts and Migrations data

# Fixed effects: year and region; Family: Gaussian
# Independent variables: 
# Dependent variable: migrants (number of migrants/population in the time interval considered)


#################################################################################################
#################################################################################################
###### PACKAGES

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer); library(xtable)} )


#################################################################################################
#################################################################################################

em <-read.csv("^Data/migr_events.csv")

setFixest_dict(c(conflicts="Type conflicts",count="Total Conflicts",confl_avg1="Avg Type Confl 1",confl_avg5="Avg Type Confl 5",count_avg1="Avg Total Confl 1",
                 count_avg5="Avg Total Confl 5", growth_count1="Growth % Total Confl 1", growth_count5="Growth % Total Confl 5",
                 growth_confl1="Growth % Type Confl 1", growth_confl5="Growth % Type Confl 5"))

#################################################################################################
#################################################################################################
######  GENERALIZED LINEAR REGRESSION FOR GLOBAL DATA

data_1 <- subset(em, interval==1)
model <- fixest::feglm(data=data_1, log(migrants)~sw(count,count_avg1,count_avg5,growth_count1)|region + year, family=gaussian)
table<- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_migr_confl1.tex", include.rownames = FALSE)

data_5 <- subset(em, interval==5)
model <- fixest::feglm(data=data_5, log(migrants)~sw(count,count_avg1,count_avg5,growth_count1,growth_count5)|region + year, family=gaussian)
table<- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_migr_confl5.tex", include.rownames = FALSE)


#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR CONTINENTS DATA (1-Y)

data_1 <- subset(em, interval==1)

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data=data_continent, log(migrants)~sw(count,count_avg1,count_avg5)|region + year, family=gaussian)
table<- xtable(etable(Africa), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_africa_1.tex", include.rownames = FALSE)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data=data_continent, log(migrants)~sw(count,count_avg1,count_avg5)|region + year, family=gaussian)
table<- xtable(etable(Asia), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_asia_1.tex", include.rownames = FALSE)

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
Europe <- fixest::feglm(data=data_continent, log(migrants)~sw(count,count_avg1,count_avg5)|region + year, family=gaussian)
table<- xtable(etable(Europe), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_europe_1.tex", include.rownames = FALSE)

North_America <- c("United States", "Canada", "Mexico")
N_A <- data_1[data_1$country %in% North_America, ]
Namerica <- fixest::feglm(data=N_A, log(migrants)~sw(count,count_avg1,count_avg5)|region + year, family=gaussian)
table<- xtable(etable(Namerica), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_Namerica_1.tex", include.rownames = FALSE)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_1[data_1$country %in% South_America, ]
Samerica <- fixest::feglm(data=S_A, log(migrants)~sw(count,count_avg1,count_avg5)|region + year, family=gaussian)
table<- xtable(etable(Samerica), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_Samerica_1.tex", include.rownames = FALSE)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_1[data_1$country %in% Central_America, ]
Camerica <- fixest::feglm(data=C_A, log(migrants)~sw(count,count_avg1,count_avg5)|region + year, family=gaussian)
table<- xtable(etable(Camerica), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_Camerica_1.tex", include.rownames = FALSE)


#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR CONTINENTS DATA (5-Y)

data_5 <- subset(gm, interval==5)

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data=data_continent, log(migrants)~sw(count,count_avg1,count_avg5,growth_count1, growth_count5)|region + year, family=gaussian)
table<- xtable(etable(Africa), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_africa_5.tex", include.rownames = FALSE)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data=data_continent, log(migrants)~sw(count,count_avg1,count_avg5,growth_count1, growth_count5)|region + year, family=gaussian)
table<- xtable(etable(Asia), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_asia_5.tex", include.rownames = FALSE)

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
Europe <- fixest::feglm(data=data_continent, log(migrants)~sw(count,count_avg1,count_avg5,growth_count1, growth_count5)|region + year, family=gaussian)
table<- xtable(etable(Europe), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_europe_5.tex", include.rownames = FALSE)

North_America <- c("United States", "Canada", "Mexico")
N_A <- data_5[data_5$country %in% North_America, ]
Namerica <- fixest::feglm(data=N_A, log(migrants)~sw(count,count_avg1,count_avg5,growth_count1, growth_count5)|region + year, family=gaussian)
table<- xtable(etable(Namerica), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_Namerica_5.tex", include.rownames = FALSE)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_5[data_5$country %in% South_America, ]
Samerica <- fixest::feglm(data=S_A, log(migrants)~sw(ccount,count_avg1,count_avg5,growth_count1, growth_count5)|region + year, family=gaussian)
table<- xtable(etable(Samerica), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_Samerica_5.tex", include.rownames = FALSE)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_5[data_5$country %in% Central_America, ]
Camerica <- fixest::feglm(data=C_A, log(migrants)~sw(count,count_avg1,count_avg5,growth_count1, growth_count5)|region + year, family=gaussian)
table<- xtable(etable(Camerica), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_cm_Camerica_5.tex", include.rownames = FALSE)




















