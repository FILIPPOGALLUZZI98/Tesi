# In this code statistical analysis is performed for GWS and Conflicts data divided by PET
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
pet <- read.csv("^Data/Separate/pet.csv")

# Setting of the dictionary for the tables
setFixest_dict(c(conflicts="# conflicts", value="gws [Kg/m^2]",
                 gws_avg1="average 1-y", gws_avg5="average 5-y", gws_avg10="average 10-y",
                 gws_logret5="log return 5-y", gws_logret10="log return 10-y",
                 gws_std1="STD 1-y", gws_std5="STD 5-y", gws_std10="STD 10-y",
                 gws_anomalies="anomalies 1y", gws_anomalies5="gws anomalies 5y",
                 gws_anomalies10="gws anomalies 10y",count="# conflicts", 
                 n_confl="normalized conflicts", n_count="normalized conflicts", n_value="normalized gws",
                 n_gws_avg1="normalized average 1-y", n_gws_avg5="normalized average 5-y",n_gws_avg10="normalized average 10-y",
                 CV1="Coefficient of variation 1-y", CV5="Coefficient of variation 5-y",CV10="Coefficient of variation 10-y"))


# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
ge <- subset(ge, type=="state")

# Create 2 classes for high and low PET
pet_l <- pet[1:140, ]; name_pet_l <- unique(pet_l$country)  ## low
pet_h <- pet[141:280, ]; name_pet_h <- unique(pet_h$country)  ## high


# Statistical model and tables

pet_H <- subset(ge, country %in% name_pet_h)
pet_high <- fixest::feglm(data=pet_H, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(pet_high); write.csv(tabella, "^Tabelle/conflicts_pet_high.csv", row.names = FALSE)

pet_L <- subset(ge, country %in% name_pet_l)
pet_low <- fixest::feglm(data=pet_L, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(pet_low); write.csv(tabella, "^Tabelle/conflicts_pet_low.csv", row.names = FALSE)


























