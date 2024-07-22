# In this code statistical analysis is performed for GLOBAL GWS and Migration data
# Generalized linear regression with fixed effects (region and year)
# Family: gaussian

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest);library(xtable); library(data.table)} )


#################################################################################################
#################################################################################################

# Upload of the groundwater-migrations dataset
gm <- read.csv("^Data/gws_migr.csv")

# Setting of the dictionary for the tables
setFixest_dict(c(migrants="# migrants/pop", value="gws [Kg/m^2]",
                 gws_avg1="average 1-y", gws_avg5="average 5-y", gws_avg10="average 10-y",
                 gws_growth1="growth rate (%) 1-y", gws_growth5="growth rate (%) 5-y", gws_growth10="growth rate (%) 10-y",
                 gws_std1="STD 1-y", gws_std5="STD 5-y", gws_std10="STD 10-y",
                 gws_anomalies="anomalies 1y", gws_anomalies5="gws anomalies 5y",
                 gws_anomalies10="gws anomalies 10y", n_value="normalized gws",
                 n_gws_avg1="normalized average 1-y", n_gws_avg5="normalized average 5-y",n_gws_avg10="normalized average 10-y",
                 CV1="Coefficient of variation 1-y", CV5="Coefficient of variation 5-y",CV10="Coefficient of variation 10-y"))


# Statistical model and tables

# 1-y Migration data
data_1 <- subset(gm, interval==1)
model <- fixest::feglm(data=data_1, log(migrants)~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_growth5,gws_growth10)|region + year, family=gaussian)
tabella <- etable(model); write.csv(tabella, "^Tabelle/migration_global_1.csv", row.names = FALSE)

# 5-y Migration data
data_5 <- subset(gm, interval==5)
model <- fixest::feglm(data=data_5, log(migrants)~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_growth5,gws_growth10)|region + year, family=gaussian)
tabella <- etable(model); write.csv(tabella, "^Tabelle/migration_global_5.csv", row.names = FALSE)


