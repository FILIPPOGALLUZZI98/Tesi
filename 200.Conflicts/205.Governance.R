# In this code statistical analysis is performed for GWS and Conflicts data divided by GOVERNANCE
# Generalized linear regression with fixed effects (region and year)
# Family: quasipoisson

#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")
gov <- read.csv("^Data/Govern.csv")
# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
ge <- subset(ge, type=="state")

# Select the governance list 
gov <- head(gov, -5)
gov <- gov[, -c(1, 2,4)]
names(gov) <- c("country", "govern")
gov$govern <- as.numeric(gov$govern)
gov <- gov %>%
  arrange(desc(govern))
gov <- na.omit(gov)

# Create 3 classes for high, medium and low governance
gov1 <- gov[1:71, ]; name_gov1 <- unique(gov1$country)  ## high
gov2 <- gov[72:142, ]; name_gov2 <- unique(gov2$country)  ## medium
gov3 <- gov[143:213, ]; name_gov3 <- unique(gov3$country)   ## low


# Statistical model and tables

ge_gov1 <- subset(ge, country %in% name_gov1)
gov1 <- fixest::feglm(data=ge_gov1, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(gov1); tabella <- confl_tabella(tabella); write.csv(tabella, "^Tabelle/conflicts_gov1.csv", row.names = FALSE)

ge_gov2 <- subset(ge, country %in% name_gov2)
gov2 <- fixest::feglm(data=ge_gov2, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(gov2);tabella <- confl_tabella(tabella);  write.csv(tabella, "^Tabelle/conflicts_gov2.csv", row.names = FALSE)

ge_gov3 <- subset(ge, country %in% name_gov3)
gov3 <- fixest::feglm(data=ge_gov3, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(gov3);tabella <- confl_tabella(tabella);  write.csv(tabella, "^Tabelle/conflicts_gov3.csv", row.names = FALSE)















