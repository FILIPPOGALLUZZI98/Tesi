# In this code statistical analysis is performed for GWS and Migration data divided by PET
# Generalized linear regression with fixed effects (region and year)
# Family: gaussian

#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
gm <- read.csv("^Data/gws_migr.csv")
pet <- read.csv("^Data/pet.csv")
gm_1 <- subset(gm, interval==1)
gm_5 <- subset(gm, interval==5)


# Create 2 classes for high and low PET
pet_l <- pet[1:140, ]; name_pet_l <- unique(pet_l$country)  ## low
pet_h <- pet[141:280, ]; name_pet_h <- unique(pet_h$country)  ## high




####  HIGH PET

pet_H <- subset(gm_1, country %in% name_pet_h)
pet_high1  <- fixest::feglm(data=pet_H, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(pet_high1)
pet_H <- subset(gm_5, country %in% name_pet_h)
pet_high5  <- fixest::feglm(data=pet_H, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(pet_high5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_pet_high.csv", row.names = FALSE)


####  LOW PET
pet_L <- subset(gm_1, country %in% name_pet_l)
pet_low1 <- fixest::feglm(data=pet_L, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(pet_low1)
pet_L <- subset(gm_5, country %in% name_pet_l)
pet_low5 <- fixest::feglm(data=pet_L, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(pet_low5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_pet_low.csv", row.names = FALSE)














