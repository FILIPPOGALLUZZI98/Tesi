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

events_sum <- subset(gem, type=="state")

####  VALUE - SUM OF EVENTS  #####
ols_c <- fixest::feols(data=events_sum, log(1+all_confl)~value|region + year)
glm_c <- fixest::feglm(data=events_sum, all_confl~value|region + year, family=quasipoisson)
ols_d <- fixest::feols(data=events_sum, log(1+all_deaths)~value|region + year)
glm_d <- fixest::feglm(data=events_sum, all_deaths~value|region + year, family=quasipoisson)
table <-etable(ols_c, glm_c, ols_d, glm_d, tex=TRUE)
write.table(table, file = "^Tables/value_sum_events.txt", sep = "\t", quote = FALSE)

####  MEAN VALUE 1 - SUM OF EVENTS  #####
ols_c <- fixest::feols(data=events_sum, log(1+all_confl)~mvalue1|region + year)
glm_c <- fixest::feglm(data=events_sum, all_confl~mvalue1|region + year, family=quasipoisson)
ols_d <- fixest::feols(data=events_sum, log(1+all_deaths)~mvalue1|region + year)
glm_d <- fixest::feglm(data=events_sum, all_deaths~mvalue1|region + year, family=quasipoisson)
table <-etable(ols_c, glm_c, ols_d, glm_d, tex=TRUE)
write.table(table, file = "^Tables/mvalue1_sum_events.txt", sep = "\t", quote = FALSE)

####  MEAN VALUE 5 - SUM OF EVENTS  #####
ols_c <- fixest::feols(data=events_sum, log(1+all_confl)~mvalue5|region + year)
glm_c <- fixest::feglm(data=events_sum, all_confl~mvalue5|region + year, family=quasipoisson)
ols_d <- fixest::feols(data=events_sum, log(1+all_deaths)~mvalue5|region + year)
glm_d <- fixest::feglm(data=events_sum, all_deaths~mvalue5|region + year, family=quasipoisson)
table <-etable(ols_c, glm_c, ols_d, glm_d, tex=TRUE)
write.table(table, file = "^Tables/mvalue5_sum_events.txt", sep = "\t", quote = FALSE)

####  VARIATION VALUE 1 - SUM OF EVENTS  #####
ols_c <- fixest::feols(data=events_sum, log(1+all_confl)~vvalue1|region + year)
glm_c <- fixest::feglm(data=events_sum, all_confl~vvalue1|region + year, family=quasipoisson)
ols_d <- fixest::feols(data=events_sum, log(1+all_deaths)~vvalue1|region + year)
glm_d <- fixest::feglm(data=events_sum, all_deaths~vvalue1|region + year, family=quasipoisson)
table <-etable(ols_c, glm_c, ols_d, glm_d, tex=TRUE)
write.table(table, file = "^Tables/vvalue1_sum_events.txt", sep = "\t", quote = FALSE)

####  VARIATION VALUE 5 - SUM OF EVENTS  #####
ols_c <- fixest::feols(data=events_sum, log(1+all_confl)~sdvalue5|region + year)
glm_c <- fixest::feglm(data=events_sum, all_confl~sdvalue5|region + year, family=quasipoisson)
ols_d <- fixest::feols(data=events_sum, log(1+all_deaths)~sdvalue5|region + year)
glm_d <- fixest::feglm(data=events_sum, all_deaths~sdvalue5|region + year, family=quasipoisson)
table <-etable(ols_c, glm_c, ols_d, glm_d, tex=TRUE)
write.table(table, file = "^Tables/vvalue5_sum_events.txt", sep = "\t", quote = FALSE)









##############################################################################################################################













