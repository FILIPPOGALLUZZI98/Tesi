#########
## GW DATA 1901-2019
## CONFLICTS DATA 1989-2019
## MIGRATION DATA START FROM 2019

##############################################################################################################################
####  CONFLICTS  ########################################################################################################################

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr)} )

events <-read.csv("^Data/gws_events.csv")
migr <- read.csv("^Data/gws_migr.csv")
gem <- read.csv("^Data/gws_migr_events.csv")


##############################################################################################################################
####  CONFLICTS  ########################################################################################################################

# All the data, not divided by type of conflict
lm <-fixest::feols(data=events, log(1+conflicts)~value|region + year)
lm2 <-fixest::feglm(data=events, conflicts~value|region + year, family=quasipoisson)


# For the type of conflict
lmN <- fixest::feols(data=subset(events, type=="Nstate"), log(1+conflicts)~value|region + year)
lm2N <- fixest::feglm(data=subset(events, type=="Nstate"), conflicts~value|region + year, family=quasipoisson)
lmS <- fixest::feols(data=subset(events, type=="state"), log(1+conflicts)~value|region + year)
lm2S <- fixest::feglm(data=subset(events, type=="state"), conflicts~value|region + year, family=quasipoisson)
lmOS <- fixest::feols(data=subset(events, type=="onesided"), log(1+conflicts)~value|region + year)
lm2OS <- fixest::feglm(data=subset(events, type=="onesided"), conflicts~value|region + year, family=quasipoisson)

results <- bind_rows(tidy(lmN), tidy(lm2N), tidy(lmS), tidy(lm2S), tidy(lmOS), tidy(lm2OS))
results$type <- c("Nstate", "Nstate", "state", "state", "onesided", "onesided")
results$model <- c("feols", "feglm", "feols", "feglm", "feols", "feglm")
print(kable(results))


# For different continents
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")
}
continent <- "Africa"
conflict_continent <- events %>%
  filter(get_continent(country) == continent)
lmAf <- fixest::feols(data=conflict_continent, log(1+conflicts)~value|region + year)
lm2Af <- fixest::feglm(data=conflict_continent, conflicts~value|region + year, family=quasipoisson)  
continent <- "Europe"
conflict_continent <- events %>%
  filter(get_continent(country) == continent)
lmE <- fixest::feols(data=conflict_continent, log(1+conflicts)~value|region + year)
lm2E <- fixest::feglm(data=conflict_continent, conflicts~value|region + year, family=quasipoisson)  
continent <- "Oceania"
conflict_continent <- events %>%
  filter(get_continent(country) == continent)
lmOc <- fixest::feols(data=conflict_continent, log(1+conflicts)~value|region + year)
lm2Oc <- fixest::feglm(data=conflict_continent, conflicts~value|region + year, family=quasipoisson)  
continent <- "Asia"
conflict_continent <- events %>%
  filter(get_continent(country) == continent)
lmAs <- fixest::feols(data=conflict_continent, log(1+conflicts)~value|region + year)
lm2As <- fixest::feglm(data=conflict_continent, conflicts~value|region + year, family=quasipoisson)  
N_A <- c("United States", "Canada", "Mexico")
S_A <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Argentina", "Bolivia", "Brasile", "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Perù", "Suriname", "Uruguay", "Venezuela", "Aruba", "Bahamas", "Barbados", "Cuba", "Dominica", "Giamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands", "British West Indies")
N_A <- events[events$country %in% N_A, ]
S_A <- events[events$country %in% S_A, ]
lmNA <- fixest::feols(data=N_A, log(1+conflicts)~value|region + year)
lm2NA <- fixest::feglm(data=N_A, conflicts~value|region + year, family=quasipoisson)
lmSA <- fixest::feols(data=S_A, log(1+conflicts)~value|region + year)
lm2SA <- fixest::feglm(data=S_A, conflicts~value|region + year, family=quasipoisson)

results_continents <- bind_rows(tidy(lmAf), tidy(lm2Af), tidy(lmE), tidy(lm2E), tidy(lmOc), tidy(lm2Oc), 
                     tidy(lmAs),tidy(lm2As),tidy(lmNA),tidy(lm2NA),tidy(lmSA),tidy(lm2SA))
results_continents$continents <- c("Africa", "Africa","Europe", "Europe","Oceania","Oceania", "Asia",  "Asia",
                        "North America", "North America","South America","South America")
results_continents$model <- c("feols", "feglm", "feols", "feglm", "feols", "feglm", "feols", "feglm","feols", "feglm","feols", "feglm")
print(kable(results_continents))

View(results_continents)


##############################################################################################################################
####  PROVE  ########################################################################################################################
data1 <- subset(gem, interval==1)
data1 <- subset(data1, type=="onesided")
data1 <- subset(data1, worldregion=="Africa & Middle East")

lm <-fixest::feols(data=data1, log(1+flow)~value|region + year); lm
lm <-fixest::feols(data=data1, log(1+flow)~mvalue1|region + year); lm
lm <-fixest::feols(data=data1, log(1+flow)~vvalue1|region + year); lm


lm2 <-fixest::feglm(data=data1, outflow_rate_annual~value|region + year, family=quasipoisson)
lm2 <-fixest::feglm(data=data1, conflicts~mvalue1|region + year, family=quasipoisson)
lm2 <-fixest::feglm(data=data1, conflicts~vvalue1|region + year, family=quasipoisson)
































