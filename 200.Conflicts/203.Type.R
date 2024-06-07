# In this code statistical analysis is performede for GWS and Conflicts data divided by TYPE
# Generalized linear regression with fixed effects (region and year)
# Family: quasipoisson

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest);library(xtable); library(data.table)} )


#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")

# Setting of the dictionary for the tables
setFixest_dict(c(conflicts="# conflicts", value="gws [Kg/m^2]",
                 gws_avg1="average 1-y", gws_avg5="average 5-y", gws_avg10="average 10-y",
                 gws_growth1="growth rate (%) 1-y", gws_growth5="growth rate (%) 5-y", gws_growth10="growth rate (%) 10-y",
                 gws_std1="STD 1-y", gws_std5="STD 5-y", gws_std10="STD 10-y",
                 gws_anomalies="anomalies 1y", gws_anomalies5="gws anomalies 5y",
                 gws_anomalies10="gws anomalies 10y",count="# conflicts", 
                 n_confl="normalized conflicts", n_count="normalized conflicts", n_value="normalized gws",
                 n_gws_avg1="normalized average 1-y", n_gws_avg5="normalized average 5-y",n_gws_avg10="normalized average 10-y",
                 CV1="Coefficient of variation 1-y", CV5="Coefficient of variation 5-y",CV10="Coefficient of variation 10-y"))


# Statistical model and tables

state <- subset(ge, type=="state")
state <- fixest::feglm(data=state, conflicts~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
n_state <- fixest::feglm(data=state, n_conflicts~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(state); df <- as.data.frame(tabella)
write.csv(tabella, "^Tabelle/conflicts_state.csv", row.names = FALSE)
n_tabella <- etable(n_state); df <- as.data.frame(n_tabella)
write.csv(n_tabella, "^Tabelle/conflicts_state_n.csv", row.names = FALSE)

Nstate <- subset(ge, type=="Nstate")
Nstate <- fixest::feglm(data=Nstate, conflicts~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
n_Nstate <- fixest::feglm(data=Nstate, n_conflicts~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(Nstate); df <- as.data.frame(tabella)
write.csv(tabella, "^Tabelle/conflicts_Nstate.csv", row.names = FALSE)
n_tabella <- etable(n_Nstate); df <- as.data.frame(n_tabella)
write.csv(n_tabella, "^Tabelle/conflicts_Nstate_n.csv", row.names = FALSE)

onesided <- subset(ge, type=="onesided")
onesided <- fixest::feglm(data=onesided, conflicts~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
n_onesided <- fixest::feglm(data=onesided, n_conflicts~sw(value, n_value,gws_avg1,gws_avg5,gws_avg10, n_gws_avg1,n_gws_avg5,n_gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, CV1, CV5, CV10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(onesided); df <- as.data.frame(tabella)
write.csv(tabella, "^Tabelle/conflicts_onesided.csv", row.names = FALSE)
n_tabella <- etable(n_onesided); df <- as.data.frame(n_tabella)
write.csv(n_tabella, "^Tabelle/conflicts_onesided_n.csv", row.names = FALSE)





