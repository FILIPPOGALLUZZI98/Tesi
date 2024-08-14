# In this code statistical analysis is performed for GLOBAL GWS and Migration data
# Generalized linear regression with fixed effects (region and year)
# Family: gaussian

#################################################################################################
#################################################################################################

# Upload of the groundwater-migrations dataset
gm <- read.csv("^Data/gws_migr.csv")


# Statistical model and tables

# 1-y Migration data
data_1 <- subset(gm, interval==1)
model <- fixest::feglm(data=data_1, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella <- etable(model); tabella <- migr_tabella(tabella); write.csv(tabella, "^Tabelle/migration_global_1.csv", row.names = FALSE)

# 5-y Migration data
data_5 <- subset(gm, interval==5)
model <- fixest::feglm(data=data_5, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella <- etable(model); tabella <- migr_tabella(tabella); write.csv(tabella, "^Tabelle/migration_global_5.csv", row.names = FALSE)


