# In this code statistical analysis is performed for GWS and Conflicts data divided by chosen Regions 
# Generalized linear regression with fixed effects (region and year)
# Family: quasipoisson

#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
gm <- read.csv("^Data/gws_migr.csv")


# List of the names of the countries
lista_1 <- c("Mauritania", "Mali", "Burkina Faso", "Niger", "Ciad", 
                 "Senegal", "Gambia", "Guinea-Bissau", "Guinea", "Costa d'Avorio",
                 "Ghana", "Nigeria", "Camerun", "Sudan", "Eritrea","Arabia Saudita", 
                  "Bahrain", "Cipro", "Egitto", "Iran", 
                         "Iraq", "Israele", "Giordania", "Kuwait", "Libano", 
                         "Oman", "Palestina", "Qatar", "Siria", "Turchia", 
                         "Emirati Arabi Uniti", "Yemen")
data_1 <- gm[gm$country %in% lista_1, ]
lista_2 <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
                      "Cabo Verde", "Camerun", "Repubblica Centrafricana", "Ciad", "Comore", 
                      "Repubblica del Congo", "Repubblica Democratica del Congo", "Costa d'Avorio", 
                      "Gibuti", "Guinea Equatoriale", "Eritrea", "Eswatini", "Etiopia", 
                      "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                      "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", 
                      "Mali", "Mauritania", "Mozambico", "Namibia", "Niger", 
                      "Nigeria", "Ruanda", "Sao Tome e Principe", "Senegal", "Seychelles", 
                      "Sierra Leone", "Somalia", "Sudan del Sud", "Sudan", "Tanzania", 
                      "Togo", "Uganda", "Zambia", "Zimbabwe")
data_2 <- gm[gm$country %in% lista_2, ]
lista_3 <- c("Brunei", "Cambogia", "Timor Est", "Indonesia", "Laos", 
                            "Malaysia", "Myanmar", "Filippine", "Singapore", "Thailandia", 
                            "Vietnam")
data_3 <- gm[gm$country %in% lista_3, ]
lista_4 <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", 
                              "Nicaragua", "Panama", "Argentina", "Bolivia", "Brasile", 
                              "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", 
                              "Peru", "Suriname", "Uruguay", "Venezuela")
data_4 <- gm[gm$country %in% lista_4, ]





####  MENA
data <- subset(data_1, interval==1)
mena <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella1 <- etable(mena)
data <- subset(data_1, interval==5)
mena <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella5 <- etable(mena)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migrations_mena.csv", row.names = FALSE)


####  SUB SAHARA
data <- subset(data_2, interval==1)
sub_sahara <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella1 <- etable(sub_sahara)
data <- subset(data_2, interval==5)
sub_sahara <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella5 <- etable(sub_sahara)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migrations_sub_sahara.csv", row.names = FALSE)


####  SUD EST ASIA
data <- subset(data_3, interval==1)
sud_est_asia <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella1 <- etable(sud_est_asia) 
data <- subset(data_3, interval==5)
sud_est_asia <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella5 <- etable(sud_est_asia)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migrations_sud_est_asia.csv", row.names = FALSE)


####  CENTRAL-SOUTH AMERICA
data <- subset(data_4, interval==1)
cs_america <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella1 <- etable(cs_america)
data <- subset(data_4, interval==5)
cs_america <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella5 <- etable(cs_america)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migrations_cs_america.csv", row.names = FALSE)











