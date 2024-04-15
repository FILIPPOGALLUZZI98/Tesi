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


state <- subset(ge, type=="state")
state <- fixest::feglm(data=state, conflicts~sw(value,gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table<- etable(state, tex=TRUE)
write.table(table, file = "^Tables/glm_conflicts_state.txt", sep = "\t", quote = FALSE, row.names = FALSE)

Nstate <- subset(ge, type=="Nstate")
Nstate <- fixest::feglm(data=Nstate, conflicts~sw(value,gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table<- etable(Nstate, tex=TRUE)
write.table(table, file = "^Tables/glm_conflicts_Nstate.txt", sep = "\t", quote = FALSE, row.names = FALSE)

onesided <- subset(ge, type=="onesided")
onesided <- fixest::feglm(data=onesided, conflicts~sw(value,gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table<- etable(onesided, tex=TRUE)
write.table(table, file = "^Tables/glm_conflicts_onesided.txt", sep = "\t", quote = FALSE, row.names = FALSE)

