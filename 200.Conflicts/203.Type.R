# In this code statistical analysis is performede for GWS and Conflicts data divided by TYPE
# Generalized linear regression with fixed effects (region and year)
# Family: quasipoisson

#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")


# Statistical model and tables

state <- subset(ge, type=="state")
model <- fixest::feglm(data=state, conflicts~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(model); tabella <- confl_tabella(tabella); write.csv(tabella, "^Tabelle/conflicts_state.csv", row.names = FALSE)

Nstate <- subset(ge, type=="Nstate")
model <- fixest::feglm(data=Nstate, conflicts~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(model); tabella <- confl_tabella(tabella); write.csv(tabella, "^Tabelle/conflicts_Nstate.csv", row.names = FALSE)

onesided <- subset(ge, type=="onesided")
model <- fixest::feglm(data=onesided, conflicts~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(model); tabella <- confl_tabella(tabella); write.csv(tabella, "^Tabelle/conflicts_onesided.csv", row.names = FALSE)





