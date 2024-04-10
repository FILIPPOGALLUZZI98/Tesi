#########
## GW DATA 1901-2019
## CONFLICTS DATA 1989-2019
## MIGRATION DATA START FROM 2019

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer)} )

gem <- read.csv("^Data/gws_migr_events.csv")


##############################################################################################################################
#### ALL CONFLICTS OLS-GLM  ####
events_sum <- subset(gem, type=="state")

## CONFLICTS OLS-GLM
ols_v <- fixest::feols(data=events_sum, log(1+all_confl)~value|region + year)
ols_mv1 <- fixest::feols(data=events_sum, log(1+all_confl)~mvalue1|region + year)
ols_mv5 <- fixest::feols(data=events_sum, log(1+all_confl)~mvalue5|region + year)
ols_vv1 <- fixest::feols(data=events_sum, log(1+all_confl)~vvalue1|region + year)
ols_vv5 <- fixest::feols(data=events_sum, log(1+all_confl)~sdvalue5|region + year)
table <-etable(ols_v, ols_mv1, ols_mv5, ols_vv1, ols_vv5, tex=TRUE)
write.table(table, file = "^Tables/ols_conflicts.txt", sep = "\t", quote = FALSE, row.names = FALSE)

glm_v <- fixest::feglm(data=events_sum, all_confl~value|region + year, family=quasipoisson)
glm_mv1 <- fixest::feglm(data=events_sum, all_confl~mvalue1|region + year, family=quasipoisson)
glm_mv5 <- fixest::feglm(data=events_sum, all_confl~mvalue5|region + year, family=quasipoisson)
glm_vv1 <- fixest::feglm(data=events_sum, all_confl~vvalue1|region + year, family=quasipoisson)
glm_vv5 <- fixest::feglm(data=events_sum, all_confl~sdvalue5|region + year, family=quasipoisson)
table <-etable(glm_v, glm_mv1, glm_mv5, glm_vv1, glm_vv5, tex=TRUE)
write.table(table, file = "^Tables/glm_conflicts.txt", sep = "\t", quote = FALSE, row.names = FALSE)


#### ALL DEATHS OLS-GLM  ####
ols_v <- fixest::feols(data=events_sum, log(1+all_deaths)~value|region + year)
ols_mv1 <- fixest::feols(data=events_sum, log(1+all_deaths)~mvalue1|region + year)
ols_mv5 <- fixest::feols(data=events_sum, log(1+all_deaths)~mvalue5|region + year)
ols_vv1 <- fixest::feols(data=events_sum, log(1+all_deaths)~vvalue1|region + year)
ols_vv5 <- fixest::feols(data=events_sum, log(1+all_deaths)~sdvalue5|region + year)
table <-etable(ols_v, ols_mv1, ols_mv5, ols_vv1, ols_vv5, tex=TRUE)
write.table(table, file = "^Tables/ols_deaths.txt", sep = "\t", quote = FALSE, row.names = FALSE)

glm_v <- fixest::feglm(data=events_sum, all_deaths~value|region + year, family=quasipoisson)
glm_mv1 <- fixest::feglm(data=events_sum, all_deaths~mvalue1|region + year, family=quasipoisson)
glm_mv5 <- fixest::feglm(data=events_sum, all_deaths~mvalue5|region + year, family=quasipoisson)
glm_vv1 <- fixest::feglm(data=events_sum, all_deaths~vvalue1|region + year, family=quasipoisson)
glm_vv5 <- fixest::feglm(data=events_sum, all_deaths~sdvalue5|region + year, family=quasipoisson)
table <-etable(glm_v, glm_mv1, glm_mv5, glm_vv1, glm_vv5, tex=TRUE)
write.table(table, file = "^Tables/glm_deaths.txt", sep = "\t", quote = FALSE, row.names = FALSE)


##############################################################################################################################
# EACH TABLES COINTAINS THE DATA FOR ONE VARIABLE AND FOR ALL OF 6 CONTINENTS

events_sum <- subset(gem, type=="state")
# CONTINENTS - VALUE
glm <- list()
world <- c("Africa", "Europe", "Oceania", "Asia")
for (continent in world){
  get_continent <- function(countries) {
    countrycode(countries, "country.name", "continent")}
  data_continent <- events_sum %>%
    filter(get_continent(country) == continent)
  a <- fixest::feglm(data = data_continent, conflicts ~ value | region + year, family = quasipoisson)
  glm[[continent]] <- a 
}
North_America <- c("United States", "Canada", "Mexico")
N_A <- gem[gem$country %in% North_America, ]
a <- fixest::feglm(data=N_A, conflicts~value|region + year, family=quasipoisson)
glm[["North_America"]] <- a
South_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands", "British West Indies")
S_A <- gem[gem$country %in% South_America, ]
a <- fixest::feglm(data=S_A, conflicts~value|region + year, family=quasipoisson)
glm[["South America"]] <- a 
table <- etable(glm, tex = TRUE)
write.table(table, file = "^Tables/glm_continents_value.txt", sep = "\t", quote = FALSE, row.names = FALSE)


# CONTINENTS - MEAN VALUE 1
glm <- list()
world <- c("Africa", "Europe", "Oceania", "Asia")
for (continent in world){
  get_continent <- function(countries) {
    countrycode(countries, "country.name", "continent")}
  data_continent <- events_sum %>%
    filter(get_continent(country) == continent)
  a <- fixest::feglm(data = data_continent, conflicts ~ mvalue1 | region + year, family = quasipoisson)
  glm[[continent]] <- a 
}
North_America <- c("United States", "Canada", "Mexico")
N_A <- gem[gem$country %in% North_America, ]
a <- fixest::feglm(data=N_A, conflicts~mvalue1|region + year, family=quasipoisson)
glm[["North_America"]] <- a
South_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands", "British West Indies")
S_A <- gem[gem$country %in% South_America, ]
a <- fixest::feglm(data=S_A, conflicts~mvalue1|region + year, family=quasipoisson)
glm[["South America"]] <- a 
table <- etable(glm, tex = TRUE)
write.table(table, file = "^Tables/glm_continents_mvalue1.txt", sep = "\t", quote = FALSE, row.names = FALSE)


# CONTINENTS - MEAN VALUE 5
glm <- list()
world <- c("Africa", "Europe", "Oceania", "Asia")
for (continent in world){
  get_continent <- function(countries) {
    countrycode(countries, "country.name", "continent")}
  data_continent <- events_sum %>%
    filter(get_continent(country) == continent)
  a <- fixest::feglm(data = data_continent, conflicts ~ mvalue5 | region + year, family = quasipoisson)
  glm[[continent]] <- a 
}
North_America <- c("United States", "Canada", "Mexico")
N_A <- events_sum[events_sum$country %in% North_America, ]
a <- fixest::feglm(data=N_A, conflicts~mvalue5|region + year, family=quasipoisson)
glm[["North_America"]] <- a
South_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands", "British West Indies")
S_A <- events_sum[events_sum$country %in% South_America, ]
a <- fixest::feglm(data=S_A, conflicts~mvalue5|region + year, family=quasipoisson)
glm[["South America"]] <- a 
table <- etable(glm, tex = TRUE)
write.table(table, file = "^Tables/glm_continents_mvalue5.txt", sep = "\t", quote = FALSE, row.names = FALSE)


# CONTINENTS - VARIATION VALUE 1
glm <- list()
world <- c("Africa", "Europe", "Oceania", "Asia")
for (continent in world){
  get_continent <- function(countries) {
    countrycode(countries, "country.name", "continent")}
  data_continent <- events_sum %>%
    filter(get_continent(country) == continent)
  a <- fixest::feglm(data = data_continent, conflicts ~ vvalue1 | region + year, family = quasipoisson)
  glm[[continent]] <- a 
}
North_America <- c("United States", "Canada", "Mexico")
N_A <- events_sum[events_sum$country %in% North_America, ]
a <- fixest::feglm(data=N_A, conflicts~vvalue1|region + year, family=quasipoisson)
glm[["North_America"]] <- a
South_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands", "British West Indies")
S_A <- events_sum[events_sum$country %in% South_America, ]
a <- fixest::feglm(data=S_A, conflicts~vvalue1|region + year, family=quasipoisson)
glm[["South America"]] <- a 
table <- etable(glm, tex = TRUE)
write.table(table, file = "^Tables/glm_continents_vvalue1.txt", sep = "\t", quote = FALSE, row.names = FALSE)


# CONTINENTS - VARIATION VALUE 5
glm <- list()
world <- c("Africa", "Europe", "Oceania", "Asia")
for (continent in world){
  get_continent <- function(countries) {
    countrycode(countries, "country.name", "continent")}
  data_continent <- events_sum %>%
    filter(get_continent(country) == continent)
  a <- fixest::feglm(data = data_continent, conflicts ~ sdvalue5 | region + year, family = quasipoisson)
  glm[[continent]] <- a 
}
North_America <- c("United States", "Canada", "Mexico")
N_A <- events_sum[events_sum$country %in% North_America, ]
a <- fixest::feglm(data=N_A, conflicts~sdvalue5|region + year, family=quasipoisson)
glm[["North_America"]] <- a
South_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands", "British West Indies")
S_A <- events_sum[events_sum$country %in% South_America, ]
a <- fixest::feglm(data=S_A, conflicts~sdvalue5|region + year, family=quasipoisson)
glm[["South America"]] <- a 
table <- etable(glm, tex = TRUE)
write.table(table, file = "^Tables/glm_continents_sdvalue5.txt", sep = "\t", quote = FALSE, row.names = FALSE)

##############################################################################################################################

data_state <- subset(gem, type=="state")
glm_v <- fixest::feglm(data=data_state, conflicts~value|region + year, family=quasipoisson)
glm_mv1 <- fixest::feglm(data=data_state, conflicts~mvalue1|region + year, family=quasipoisson)
glm_mv5 <- fixest::feglm(data=data_state, conflicts~mvalue5|region + year, family=quasipoisson)
glm_vv1 <- fixest::feglm(data=data_state, conflicts~vvalue1|region + year, family=quasipoisson)
glm_vv5 <- fixest::feglm(data=data_state, conflicts~sdvalue5|region + year, family=quasipoisson)
table <-etable(glm_v, glm_mv1, glm_mv5, glm_vv1, glm_vv5, tex=TRUE)
write.table(table, file = "^Tables/glm_confl_state.txt", sep = "\t", quote = FALSE, row.names = FALSE)

data_onesided <- subset(gem, type=="onesided")
glm_v <- fixest::feglm(data=data_onesided, conflicts~value|region + year, family=quasipoisson)
glm_mv1 <- fixest::feglm(data=data_onesided, conflicts~mvalue1|region + year, family=quasipoisson)
glm_mv5 <- fixest::feglm(data=data_onesided, conflicts~mvalue5|region + year, family=quasipoisson)
glm_vv1 <- fixest::feglm(data=data_onesided, conflicts~vvalue1|region + year, family=quasipoisson)
glm_vv5 <- fixest::feglm(data=data_onesided, conflicts~sdvalue5|region + year, family=quasipoisson)
table <-etable(glm_v, glm_mv1, glm_mv5, glm_vv1, glm_vv5, tex=TRUE)
write.table(table, file = "^Tables/glm_confl_onesided.txt", sep = "\t", quote = FALSE, row.names = FALSE)

data_Nstate <- subset(gem, type=="Nstate")
glm_v <- fixest::feglm(data=data_Nstate, conflicts~value|region + year, family=quasipoisson)
glm_mv1 <- fixest::feglm(data=data_Nstate, conflicts~mvalue1|region + year, family=quasipoisson)
glm_mv5 <- fixest::feglm(data=data_Nstate, conflicts~mvalue5|region + year, family=quasipoisson)
glm_vv1 <- fixest::feglm(data=data_Nstate, conflicts~vvalue1|region + year, family=quasipoisson)
glm_vv5 <- fixest::feglm(data=data_Nstate, conflicts~sdvalue5|region + year, family=quasipoisson)
table <-etable(glm_v, glm_mv1, glm_mv5, glm_vv1, glm_vv5, tex=TRUE)
write.table(table, file = "^Tables/glm_confl_Nstate.txt", sep = "\t", quote = FALSE, row.names = FALSE)


##############################################################################################################################

world <- c("Africa", "Europe", "Oceania", "Asia")

for (continent in world){
  get_continent <- function(countries) {
    countrycode(countries, "country.name", "continent")}
  data_state <- subset(gem, country==continent)
  data_continent <- gem %>%
    filter(get_continent(country)==continent)
  glm_v <- fixest::feglm(data=data_continent, conflicts~value|region + year, family=quasipoisson)
  glm_mv1 <- fixest::feglm(data=data_continent, conflicts~mvalue1|region + year, family=quasipoisson)
  glm_mv5 <- fixest::feglm(data=data_continent, conflicts~mvalue5|region + year, family=quasipoisson)
  glm_vv1 <- fixest::feglm(data=data_continent, conflicts~vvalue1|region + year, family=quasipoisson)
  glm_vv5 <- fixest::feglm(data=data_continent, conflicts~sdvalue5|region + year, family=quasipoisson)
  table <-etable(glm_v, glm_mv1, glm_mv5, glm_vv1, glm_vv5, tex=TRUE)
  write.table(table, file = paste0("^Tables/glm_confl_",continent, ".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
}

North_America <- c("United States", "Canada", "Mexico")
N_A <- gem[gem$country %in% North_America, ]
glm_v <- fixest::feglm(data=N_A, conflicts~value|region + year, family=quasipoisson)
glm_mv1 <- fixest::feglm(data=N_A, conflicts~mvalue1|region + year, family=quasipoisson)
glm_mv5 <- fixest::feglm(data=N_A, conflicts~mvalue5|region + year, family=quasipoisson)
glm_vv1 <- fixest::feglm(data=N_A, conflicts~vvalue1|region + year, family=quasipoisson)
glm_vv5 <- fixest::feglm(data=N_A, conflicts~sdvalue5|region + year, family=quasipoisson)
table <-etable(glm_v, glm_mv1, glm_mv5, glm_vv1, glm_vv5, tex=TRUE)
write.table(table, file = "^Tables/glm_confl_North_America.txt", sep = "\t", quote = FALSE, row.names = FALSE)


South_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands", "British West Indies")
S_A <- gem[gem$country %in% South_America, ]
glm_v <- fixest::feglm(data=S_A, conflicts~value|region + year, family=quasipoisson)
glm_mv1 <- fixest::feglm(data=S_A, conflicts~mvalue1|region + year, family=quasipoisson)
glm_mv5 <- fixest::feglm(data=S_A, conflicts~mvalue5|region + year, family=quasipoisson)
glm_vv1 <- fixest::feglm(data=S_A, conflicts~vvalue1|region + year, family=quasipoisson)
glm_vv5 <- fixest::feglm(data=S_A, conflicts~sdvalue5|region + year, family=quasipoisson)
table <-etable(glm_v, glm_mv1, glm_mv5, glm_vv1, glm_vv5, tex=TRUE)
write.table(table, file = "^Tables/glm_confl_South_America.txt", sep = "\t", quote = FALSE, row.names = FALSE)

