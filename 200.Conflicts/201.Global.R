# In this code statistical analysis is performed for GWS and Conflicts data

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
setFixest_dict(c(conflicts="# conflicts", value="gws [g/m^2]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies 1y (1980-2010)", gws_anomalies5="gws anomalies 5y (1980-2010)",
                 gws_anomalies10="gws anomalies 10y (1980-2010)",count="# conflict"))


#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR GLOBAL DATA

# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
events_sum <- subset(ge, type=="state" & year>1988)

model <- fixest::feglm(data=events_sum, count~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
table <- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts.tex", include.rownames = FALSE)


#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR TYPE OF CONFLICTS DATA

state <- subset(ge, type=="state" & year>1988)
state <- fixest::feglm(data=state, conflicts~sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table_state<- xtable(etable(state), tex=TRUE)
print(table_state, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_state.tex", include.rownames = FALSE)

Nstate <- subset(ge, type=="Nstate" & year>1988)
Nstate <- fixest::feglm(data=Nstate, conflicts~sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table_Nstate<- xtable(etable(Nstate), tex=TRUE)
print(table_Nstate, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_Nstate.tex", include.rownames = FALSE)

onesided <- subset(ge, type=="onesided" & year>1988)
onesided <- fixest::feglm(data=onesided, conflicts~sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table_onesided<- xtable(etable(onesided), tex=TRUE)
print(table_onesided, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_onesided.tex", include.rownames = FALSE)























