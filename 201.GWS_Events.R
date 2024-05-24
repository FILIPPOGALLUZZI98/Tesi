# In this code statistical analysis is performed

# Fixed effects: year and region; Family: Quasipoisson
# Independent variables: gws (average 1-5-10 y), gws anomalies (average 1-5-10 y), gws standard deviations (1-5-10 y)
# gws growth rate percentage (1-5-10 y)
# Dependent variable: count (number of conflicts of all types, per each year and region)


#################################################################################################
#################################################################################################
###### PACKAGES

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer); library(xtable)} )


#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")

# Setting of the dictionary for the tables
setFixest_dict(c(conflicts="Type conflicts", value="gws [g/m^2]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies 1y (1980-2010)", gws_anomalies5="gws anomalies 5y (1980-2010)",
                 gws_anomalies10="gws anomalies 10y (1980-2010)",count="total conflict"))


#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR GLOBAL DATA

# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
events_sum <- subset(ge, type=="state")

model <- fixest::feglm(data=events_sum, count~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
table <- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts.tex", include.rownames = FALSE)


#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR CONTINENTS DATA

# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
events_sum <- subset(ge, type=="state")

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data = data_continent, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_africa <- xtable(etable(Africa), tex=TRUE)
print(table_africa, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_africa.tex", include.rownames = FALSE)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data = data_continent, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_asia <- xtable(etable(Asia), tex=TRUE)
print(table_asia, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_asia.tex", include.rownames = FALSE)

continent <- "Oceania"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Oceania <- fixest::feglm(data = data_continent, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_oceania <- xtable(etable(Oceania), tex=TRUE)
print(table_oceania, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_oceania.tex", include.rownames = FALSE)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data = data_continent, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_europe <- xtable(etable(Europe), tex=TRUE)
print(table_europe, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_europe.tex", include.rownames = FALSE)

North_America <- c("United States", "Canada", "Mexico")
N_A <- events_sum[events_sum$country %in% North_America, ]
Namerica <- fixest::feglm(data = N_A, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_Namerica <- xtable(etable(Namerica), tex=TRUE)
print(table_Namerica, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_Namerica.tex", include.rownames = FALSE)

South_America <- c("Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- events_sum[events_sum$country %in% South_America, ]
Samerica <- fixest::feglm(data = S_A, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_Samerica <- xtable(etable(Samerica), tex=TRUE)
print(table_Samerica, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_Samerica.tex", include.rownames = FALSE)

Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- events_sum[events_sum$country %in% Central_America, ]
Camerica <- fixest::feglm(data = C_A, count ~ sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10, gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10) | region + year, family = quasipoisson)
table_Camerica <- xtable(etable(Camerica), tex=TRUE)
print(table_Camerica, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_Camerica.tex", include.rownames = FALSE)


#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR TYPE OF CONFLICTS DATA

state <- subset(ge, type=="state")
state <- fixest::feglm(data=state, conflicts~sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table_state<- xtable(etable(state), tex=TRUE)
print(table_state, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_state.tex", include.rownames = FALSE)

Nstate <- subset(ge, type=="Nstate")
Nstate <- fixest::feglm(data=Nstate, conflicts~sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table_Nstate<- xtable(etable(Nstate), tex=TRUE)
print(table_Nstate, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_Nstate.tex", include.rownames = FALSE)

onesided <- subset(ge, type=="onesided")
onesided <- fixest::feglm(data=onesided, conflicts~sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table_onesided<- xtable(etable(onesided), tex=TRUE)
print(table_onesided, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_onesided.tex", include.rownames = FALSE)























