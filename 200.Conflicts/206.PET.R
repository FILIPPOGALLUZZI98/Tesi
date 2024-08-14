# In this code statistical analysis is performed for GWS and Conflicts data divided by PET
# Generalized linear regression with fixed effects (region and year)
# Family: quasipoisson

#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")
pet <- read.csv("^Data/pet.csv")
# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
ge <- subset(ge, type=="state")

# Create 2 classes for high and low PET
pet_l <- pet[1:140, ]; name_pet_l <- unique(pet_l$country)  ## low
pet_h <- pet[141:280, ]; name_pet_h <- unique(pet_h$country)  ## high


# Statistical model and tables

pet_H <- subset(ge, country %in% name_pet_h)
pet_high <- fixest::feglm(data=pet_H, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(pet_high); tabella <- confl_tabella(tabella); write.csv(tabella, "^Tabelle/conflicts_pet_high.csv", row.names = FALSE)

pet_L <- subset(ge, country %in% name_pet_l)
pet_low <- fixest::feglm(data=pet_L, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(pet_low); tabella <- confl_tabella(tabella); write.csv(tabella, "^Tabelle/conflicts_pet_low.csv", row.names = FALSE)


























