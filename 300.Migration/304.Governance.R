# In this code statistical analysis is performed for GWS and Migration data divided by GOVERNANCE
# Generalized linear regression with fixed effects (region and year)
# Family: quasipoisson

#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
gm <- read.csv("^Data/gws_migr.csv")
govern <- read.csv("^Data/Govern.csv")
gm_1 <- subset(gm, interval==1)
gm_5 <- subset(gm, interval==5)


# Select the governance list 
govern <- govern %>%
  slice(1:(n() - 7)) %>%
  select(-c(1,2,4)) %>%
  rename(
    country = Country.Name,
    gov = Government.Effectiveness..Estimate..GE.EST.)
govern$gov <- as.numeric(govern$gov)
govern <- govern %>%
  arrange(desc(gov))
govern <- govern[-214, ]

# Create 3 classes for high, medium and low governance
gov1 <- govern[1:71, ]; name_gov1 <- unique(gov1$country)  ## high
gov2 <- govern[72:142, ]; name_gov2 <- unique(gov2$country)  ## medium
gov3 <- govern[143:213, ]; name_gov3 <- unique(gov3$country)   ## low



####  LOW GOVERNANCE
gm_gov1 <- subset(gm_1, country %in% name_gov1)
gov11 <- fixest::feglm(data=gm_gov1, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(gov11)
gm_gov1 <- subset(gm_5, country %in% name_gov1)
gov15 <- fixest::feglm(data=gm_gov1, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(gov15)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_gov1.csv", row.names = FALSE)


####  MID GOVERNANCE
gm_gov2 <- subset(gm_1, country %in% name_gov2)
gov21 <- fixest::feglm(data=gm_gov2, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(gov21); write.csv(tabella, "^Tabelle/migration_gov2_1.csv", row.names = FALSE)
gm_gov2 <- subset(gm_5, country %in% name_gov2)
gov25 <- fixest::feglm(data=gm_gov2, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(gov25)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_gov2.csv", row.names = FALSE)


####  HIGH GOVERNANCE
gm_gov3 <- subset(gm_1, country %in% name_gov3)
gov31 <- fixest::feglm(data=gm_gov3, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(gov31)
gm_gov3 <- subset(gm_5, country %in% name_gov3)
gov35 <- fixest::feglm(data=gm_gov3, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(gov35)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_gov3.csv", row.names = FALSE)











