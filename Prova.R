#########
## GW DATA 1901-2019
## CONFLICTS DATA 1989-2019
## MIGRATION DATA START FROM 2019

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer)} )

gem <- read.csv("^Data/gws_migr_events.csv")

head(gem)
##############################################################################################################################
####  CONFLICTS  #####























ols_c <- fixest::feols(data=gem, log(1+all_confl)~value|region + year)
glm_c <- fixest::feglm(data=gem, conflicts~value|region + year, family=quasipoisson)
ols_cm1 <- fixest::feols(data=gem, log(1+conflicts)~mvalue1|region + year)
glm_cm1 <- fixest::feglm(data=gem, conflicts~mvalue1|region + year, family=quasipoisson)
ols_cm5 <- fixest::feols(data=gem, log(1+conflicts)~mvalue5|region + year)
glm_cm5 <- fixest::feglm(data=gem, conflicts~mvalue5|region + year, family=quasipoisson)
ols_cv1 <- fixest::feols(data=gem, log(1+conflicts)~vvalue1|region + year)
glm_cv1 <- fixest::feglm(data=gem, conflicts~vvalue1|region + year, family=quasipoisson)
ols_cv5 <- fixest::feols(data=gem, log(1+conflicts)~sdvalue5|region + year)
glm_cv5 <- fixest::feglm(data=gem, conflicts~sdvalue5|region + year, family=quasipoisson)



etable(ols_c, glm_c, ols_cm1,glm_cm1, ols_cm5,glm_cm5, ols_cv1, glm_cv1,ols_cv5, glm_cv5)

