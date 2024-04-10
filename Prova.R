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
#### ALL CONFLICTS  ####
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


## DEATHS OLS-GLM
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
write.table(table, file = "^Tables/glm_conflicts.txt", sep = "\t", quote = FALSE, row.names = FALSE)







##############################################################################################################################













