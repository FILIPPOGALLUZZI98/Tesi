#########
## GW DATA 1901-2019
## CONFLICTS DATA 1989-2019
## MIGRATION DATA START FROM 2019

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer)} )

gem <- read.csv("^Data/gws_migr_events.csv")

setFixest_dict(c(all_confl="# of conflicts",value = "GWS", mvalue1 = "GWS 1-year average ",vvalue1="GWS 1-year variation",growth_value1="% 1-year GWS variation",
       mvalue5="GWS 5-year average", growth_value5="% 5-year GWS variation", sdvalue5="GWS std 5-year", anomaly_it="GWS Anomalies 1980-2010 "))

##############################################################################################################################
#### ALL CONFLICTS GLM  ####
events_sum <- subset(gem, type=="state")

gws1 <- fixest::feglm(data=events_sum, all_confl~sw(value,mvalue1,vvalue1,growth_value1)|region + year, family=quasipoisson)
gws2 <- fixest::feglm(data=events_sum, all_confl~sw(mvalue5,growth_value5,sdvalue5,anomaly_it)|region + year, family=quasipoisson)
table1<- etable(gws1)
table2<- etable(gws2)

##############################################################################################################################
#### ALL CONFLICTS - CONTINENTS GLM  ####

continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data = data_continent, all_confl ~ sw(value,mvalue1,growth_value1,mvalue5,growth_value5,sdvalue5,anomaly_it) | region + year, family = quasipoisson)
table_africa <- etable(Africa)

continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data = data_continent, all_confl ~ sw(value,mvalue1,growth_value1,mvalue5,growth_value5,sdvalue5,anomaly_it) | region + year, family = quasipoisson)
table_Asia <- etable(Asia)

continent <- "Oceania"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Oceania <- fixest::feglm(data = data_continent, all_confl ~ sw(value,mvalue1,growth_value1,mvalue5,growth_value5,sdvalue5,anomaly_it) | region + year, family = quasipoisson)
table_oceania <- etable(Oceania)

continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data = data_continent, all_confl ~ sw(value,mvalue1,growth_value1,mvalue5,growth_value5,sdvalue5,anomaly_it) | region + year, family = quasipoisson)
table_europe <- etable(Europe)

##############################################################################################################################
#### ALL CONFLICTS GLM  ####

state <- subset(gem, type=="state")
state1 <- fixest::feglm(data=state, conflicts~sw(value,mvalue1,vvalue1,growth_value1)|region + year, family=quasipoisson)
state2 <- fixest::feglm(data=state, conflicts~sw(mvalue5,growth_value5,sdvalue5,anomaly_it)|region + year, family=quasipoisson)
table1<- etable(state1)
table2<- etable(state2)

Nstate <- subset(gem, type=="Nstate")
Nstate1 <- fixest::feglm(data=Nstate, conflicts~sw(value,mvalue1,vvalue1,growth_value1)|region + year, family=quasipoisson)
Nstate2 <- fixest::feglm(data=Nstate, conflicts~sw(mvalue5,growth_value5,sdvalue5,anomaly_it)|region + year, family=quasipoisson)
table1<- etable(Nstate1)
table2<- etable(Nstate2)

onesided <- subset(gem, type=="onesided")
onesided1 <- fixest::feglm(data=onesided, conflicts~sw(value,mvalue1,vvalue1,growth_value1)|region + year, family=quasipoisson)
onesided2 <- fixest::feglm(data=onesided, conflicts~sw(mvalue5,growth_value5,sdvalue5,anomaly_it)|region + year, family=quasipoisson)
table1<- etable(onesided1)
table2<- etable(onesided2)


write.table(table, file = "^Tables/glm_conflicts.txt", sep = "\t", quote = FALSE, row.names = FALSE)






















