# In this code statistical analysis is performed for GWS and Migration data divided by GOVERNANCE
# Generalized linear regression with fixed effects (region and year)
# Family: quasipoisson

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest);library(xtable); library(data.table)} )


#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
gm <- read.csv("^Data/gws_migr.csv")
govern <- read.csv("^Data/Govern.csv")

# Setting of the dictionary for the tables
setFixest_dict(c(n_migr="norm #migrants/pop", value="gws [Kg/m^2]",
                 gws_avg1="average 1-y", gws_avg5="average 5-y", gws_avg10="average 10-y",
                 gws_logret5="log return 5-y", gws_logret10="log return 10-y",
                 gws_std1="STD 1-y", gws_std5="STD 5-y", gws_std10="STD 10-y",
                 gws_anomalies="anomalies 1y", gws_anomalies5="gws anomalies 5y",
                 gws_anomalies10="gws anomalies 10y", n_value="normalized gws",
                 n_gws_avg1="normalized average 1-y", n_gws_avg5="normalized average 5-y",n_gws_avg10="normalized average 10-y",
                 CV1="Coefficient of variation 1-y", CV5="Coefficient of variation 5-y",CV10="Coefficient of variation 10-y"))


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



####  ANALYSIS FOR 1-Y INTERVAL
gm_1 <- subset(gm, interval==1)

gm_gov1 <- subset(gm_1, country %in% name_gov1)
gov1 <- fixest::feglm(data=gm_gov1, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella <- etable(gov1); write.csv(tabella, "^Tabelle/migration_gov1_1.csv", row.names = FALSE)

gm_gov2 <- subset(gm_1, country %in% name_gov2)
gov2 <- fixest::feglm(data=gm_gov2, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella <- etable(gov2); write.csv(tabella, "^Tabelle/migration_gov2_1.csv", row.names = FALSE)

gm_gov3 <- subset(gm_1, country %in% name_gov3)
gov3 <- fixest::feglm(data=gm_gov3, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella <- etable(gov3); write.csv(tabella, "^Tabelle/migration_gov3_1.csv", row.names = FALSE)



####  ANALYSIS FOR 5-Y INTERVAL
gm_5 <- subset(gm, interval==5)

gm_gov1 <- subset(gm_5, country %in% name_gov1)
gov1 <- fixest::feglm(data=gm_gov1, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella <- etable(gov1); write.csv(tabella, "^Tabelle/migration_gov1_5.csv", row.names = FALSE)

gm_gov2 <- subset(gm_5, country %in% name_gov2)
gov2 <- fixest::feglm(data=gm_gov2, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella <- etable(gov2); write.csv(tabella, "^Tabelle/migration_gov2_5.csv", row.names = FALSE)

gm_gov3 <- subset(gm_5, country %in% name_gov3)
gov3 <- fixest::feglm(data=gm_gov3, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella <- etable(gov3); write.csv(tabella, "^Tabelle/migration_gov3_5.csv", row.names = FALSE)











