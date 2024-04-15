suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer)} )

ge <- read.csv("^Data/gws_events.csv")

setFixest_dict(c(conflicts="number of conflicts per type", value="groundwater storage [g/m^3]",
                 gws_avg1="groundwater storage average 1-y", gws_avg5="groundwater storage average 5-y", gws_avg10="groundwater storage average 10-y",
                 gws_growth1="groundwater growth rate 1-y", gws_growth5="groundwater growth rate 5-y", gws_growth10="groundwater growth rate 10-y",
                 gws_std1="groundwater standard deviation 1-y", gws_std5="groundwater standard deviation 5-y", gws_std10="groundwater standard deviation 10-y",
                 gws_anomalies="groundwater anomalies (1980-2010)", count="total number of conflict per year"))

events_sum <- subset(ge, type=="state")

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data = data_continent, count ~ sw(value,gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_africa <- etable(Africa, tex=TRUE)
write.table(table_africa, file = "^Tables/glm_conflicts_africa.txt", sep = "\t", quote = FALSE, row.names = FALSE)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data = data_continent, count ~ sw(value,gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_asia <- etable(Asia, tex=TRUE)
write.table(table_asia, file = "^Tables/glm_conflicts_asia.txt", sep = "\t", quote = FALSE, row.names = FALSE)

continent <- "Oceania"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Oceania <- fixest::feglm(data = data_continent, count ~ sw(value,gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_oceania <- etable(Oceania, tex=TRUE)
write.table(table_oceania, file = "^Tables/glm_conflicts_oceania.txt", sep = "\t", quote = FALSE, row.names = FALSE)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data = data_continent, count ~ sw(value,gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_europe <- etable(Europe, tex=TRUE)
write.table(table_europe, file = "^Tables/glm_conflicts_europe.txt", sep = "\t", quote = FALSE, row.names = FALSE)

North_America <- c("United States", "Canada", "Mexico")
N_A <- events_sum[events_sum$country %in% North_America, ]
Namerica <- fixest::feglm(data = N_A, count ~ sw(value,gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_Namerica <- etable(Namerica, tex=TRUE)
write.table(table_Namerica, file = "^Tables/glm_conflicts_Namerica.txt", sep = "\t", quote = FALSE, row.names = FALSE)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- events_sum[events_sum$country %in% South_America, ]
Samerica <- fixest::feglm(data = S_A, count ~ sw(value,gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_Samerica <- etable(Samerica, tex=TRUE)
write.table(table_Samerica, file = "^Tables/glm_conflicts_Samerica.txt", sep = "\t", quote = FALSE, row.names = FALSE)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- events_sum[events_sum$country %in% Central_America, ]
Camerica <- fixest::feglm(data = C_A, count ~ sw(value,gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_Camerica <- etable(Camerica, tex=TRUE)
write.table(table_Camerica, file = "^Tables/glm_conflicts_Camerica.txt", sep = "\t", quote = FALSE, row.names = FALSE)





