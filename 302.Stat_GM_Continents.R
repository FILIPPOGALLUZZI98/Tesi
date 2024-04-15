suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer)} )

gm <- read.csv("^Data/gws_migr.csv")

setFixest_dict(c(migrants="number of migrants/population in the interval", value="groundwater storage [g/m^3]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies (1980-2010)"))

#################################################################################################
##### 1-YEAR  ####################################################################################

data_1 <- subset(gm, interval==1)

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feols(data=data_continent, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_africa <- etable(Africa, tex=TRUE)
write.table(table_africa, file = "^Tables/glm_migration_africa_1.txt", sep = "\t", quote = FALSE, row.names = FALSE)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feols(data=data_continent, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_asia <- etable(Asia, tex=TRUE)
write.table(table_asia, file = "^Tables/glm_migration_asia_1.txt", sep = "\t", quote = FALSE, row.names = FALSE)

continent <- "Oceania"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Oceania <- fixest::feols(data=data_continent, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_oceania <- etable(Oceania, tex=TRUE)
write.table(table_oceania, file = "^Tables/glm_migration_oceania_1.txt", sep = "\t", quote = FALSE, row.names = FALSE)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feols(data=data_continent, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_europe <- etable(Europe, tex=TRUE)
write.table(table_europe, file = "^Tables/glm_migration_europe_1.txt", sep = "\t", quote = FALSE, row.names = FALSE)

North_America <- c("United States", "Canada", "Mexico")
N_A <- data_1[data_1$country %in% North_America, ]
Namerica <- fixest::feols(data=N_A, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_Namerica <- etable(Namerica, tex=TRUE)
write.table(table_Namerica, file = "^Tables/glm_migration_Namerica_1.txt", sep = "\t", quote = FALSE, row.names = FALSE)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_1[data_1$country %in% South_America, ]
Samerica <- fixest::feols(data=S_A, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_Samerica <- etable(Samerica, tex=TRUE)
write.table(table_Samerica, file = "^Tables/glm_migration_Samerica_1.txt", sep = "\t", quote = FALSE, row.names = FALSE)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_1[data_1$country %in% Central_America, ]
Camerica <- fixest::feols(data=C_A, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_Camerica <- etable(Camerica, tex=TRUE)
write.table(table_Camerica, file = "^Tables/glm_migration_Camerica_1.txt", sep = "\t", quote = FALSE, row.names = FALSE)


#################################################################################################
##### 5-YEAR  ####################################################################################

data_5 <- subset(gm, interval==5)

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feols(data=data_continent, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_africa <- etable(Africa, tex=TRUE)
write.table(table_africa, file = "^Tables/glm_migration_africa_5.txt", sep = "\t", quote = FALSE, row.names = FALSE)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feols(data=data_continent, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_asia <- etable(Asia, tex=TRUE)
write.table(table_asia, file = "^Tables/glm_migration_asia_5.txt", sep = "\t", quote = FALSE, row.names = FALSE)

continent <- "Oceania"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Oceania <- fixest::feols(data=data_continent, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_oceania <- etable(Oceania, tex=TRUE)
write.table(table_oceania, file = "^Tables/glm_migration_oceania_5.txt", sep = "\t", quote = FALSE, row.names = FALSE)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feols(data=data_continent, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_europe <- etable(Europe, tex=TRUE)
write.table(table_europe, file = "^Tables/glm_migration_europe_5.txt", sep = "\t", quote = FALSE, row.names = FALSE)

North_America <- c("United States", "Canada", "Mexico")
N_A <- data_5[data_5$country %in% North_America, ]
Namerica <- fixest::feols(data=N_A, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_Namerica <- etable(Namerica, tex=TRUE)
write.table(table_Namerica, file = "^Tables/glm_migration_Namerica_5.txt", sep = "\t", quote = FALSE, row.names = FALSE)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_5[data_5$country %in% South_America, ]
Samerica <- fixest::feols(data=S_A, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_Samerica <- etable(Samerica, tex=TRUE)
write.table(table_Samerica, file = "^Tables/glm_migration_Samerica_5.txt", sep = "\t", quote = FALSE, row.names = FALSE)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_5[data_5$country %in% Central_America, ]
Camerica <- fixest::feols(data=C_A, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table_Camerica <- etable(Camerica, tex=TRUE)
write.table(table_Camerica, file = "^Tables/glm_migration_Camerica_5.txt", sep = "\t", quote = FALSE, row.names = FALSE)
















