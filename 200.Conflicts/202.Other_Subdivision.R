# In this code statistical analysis is performed for GWS and Conflicts data divided by chosen Regions 
# Generalized linear regression with fixed effects (region and year)
# Family: quasipoisson

#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")
# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
events_sum <- subset(ge, type=="state")

# List of the names of the countries
lista_1 <- c("Mauritania", "Mali", "Burkina Faso", "Niger", "Chad", 
             "Senegal", "Gambia", "Guinea-Bissau", "Guinea", "Ivory Coast",
             "Ghana", "Nigeria", "Cameroon", "Sudan", "Eritrea", "Saudi Arabia", 
             "Bahrain", "Cyprus", "Egypt", "Iran", 
             "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", 
             "Oman", "Palestine", "Qatar", "Syria", "Turkey", 
             "United Arab Emirates", "Yemen")
data_1 <- events_sum[events_sum$country %in% lista_1, ]
lista_2 <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
             "Cape Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", 
             "Republic of the Congo", "Democratic Republic of the Congo", "Ivory Coast", 
             "Djibouti", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", 
             "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
             "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", 
             "Mali", "Mauritania", "Mozambique", "Namibia", "Niger", 
             "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", 
             "Sierra Leone", "Somalia", "South Sudan", "Sudan", "Tanzania", 
             "Togo", "Uganda", "Zambia", "Zimbabwe")
data_2 <- events_sum[events_sum$country %in% lista_2, ]
lista_3 <- c("Brunei", "Cambodia", "East Timor", "Indonesia", "Laos", 
             "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", 
             "Vietnam")
data_3 <- events_sum[events_sum$country %in% lista_3, ]
lista_4 <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", 
             "Nicaragua", "Panama", "Argentina", "Bolivia", "Brazil", 
             "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", 
             "Peru", "Suriname", "Uruguay", "Venezuela")
data_4 <- events_sum[events_sum$country %in% lista_4, ]

# Statistical model and tables

mena <- fixest::feglm(data = data_1, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(mena); tabella <- confl_tabella(tabella); write.csv(tabella, "^Tabelle/conflicts_mena.csv", row.names = FALSE)

sub_sahara <- fixest::feglm(data = data_2, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(sub_sahara); tabella <- confl_tabella(tabella); write.csv(tabella, "^Tabelle/conflicts_sub_sahara.csv", row.names = FALSE)

sud_est_asia <- fixest::feglm(data = data_3, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(sud_est_asia); tabella <- confl_tabella(tabella); write.csv(tabella, "^Tabelle/conflicts_sud_est_asia.csv", row.names = FALSE)

cs_america <- fixest::feglm(data = data_4, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(cs_america); tabella <- confl_tabella(tabella); write.csv(tabella, "^Tabelle/conflicts_cs_america.csv", row.names = FALSE)






























