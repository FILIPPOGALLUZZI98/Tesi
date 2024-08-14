# In this code statistical analysis is performed for GLOBAL GWS and Conflicts data
# Generalized linear regression with fixed effects (region and year)
# Family: quasipoisson

#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")

# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
events_sum <- subset(ge, type=="state")

# Statistical model and table (for normalized variables)
model <- fixest::feglm(data=events_sum, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(model);tabella <- confl_tabella(tabella); write.csv(tabella, "^Tabelle/conflicts_global.csv", row.names = FALSE)
























