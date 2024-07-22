# In this code statistical analysis is performed for GWS and Conflicts data divided by chosen Regions 
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

# Setting of the dictionary for the tables
setFixest_dict(c(conflicts="# conflicts", value="gws [Kg/m^2]",
                 gws_avg1="average 1-y", gws_avg5="average 5-y", gws_avg10="average 10-y",
                 gws_growth1="growth rate (%) 1-y", gws_growth5="growth rate (%) 5-y", gws_growth10="growth rate (%) 10-y",
                 gws_std1="STD 1-y", gws_std5="STD 5-y", gws_std10="STD 10-y",
                 gws_anomalies="anomalies 1y", gws_anomalies5="gws anomalies 5y",
                 gws_anomalies10="gws anomalies 10y",count="# conflicts", 
                 n_confl="normalized conflicts", n_count="normalized conflicts", n_value="normalized gws",
                 n_gws_avg1="normalized average 1-y", n_gws_avg5="normalized average 5-y",n_gws_avg10="normalized average 10-y",
                 CV1="Coefficient of variation 1-y", CV5="Coefficient of variation 5-y",CV10="Coefficient of variation 10-y"))


# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
events_sum <- subset(ge, type=="state")

# List of the names of the countries
lista_1 <- c("Mauritania", "Mali", "Burkina Faso", "Niger", "Ciad", 
                 "Senegal", "Gambia", "Guinea-Bissau", "Guinea", "Costa d'Avorio",
                 "Ghana", "Nigeria", "Camerun", "Sudan", "Eritrea","Arabia Saudita", 
                  "Bahrain", "Cipro", "Egitto", "Iran", 
                         "Iraq", "Israele", "Giordania", "Kuwait", "Libano", 
                         "Oman", "Palestina", "Qatar", "Siria", "Turchia", 
                         "Emirati Arabi Uniti", "Yemen")
data_1 <- events_sum[events_sum$country %in% lista_1, ]
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
data_2 <- events_sum[events_sum$country %in% lista_2, ]
lista_3 <- c("Brunei", "Cambogia", "Timor Est", "Indonesia", "Laos", 
                            "Malaysia", "Myanmar", "Filippine", "Singapore", "Thailandia", 
                            "Vietnam")
data_3 <- events_sum[events_sum$country %in% lista_3, ]
lista_4 <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", 
                              "Nicaragua", "Panama", "Argentina", "Bolivia", "Brasile", 
                              "Cile", "Colombia", "Ecuador", "Guyana", "Paraguay", 
                              "Peru", "Suriname", "Uruguay", "Venezuela")
data_4 <- events_sum[events_sum$country %in% lista_4, ]


# Statistical model and tables

mena <- fixest::feglm(data = data_1, count~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_growth5,gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(mena); write.csv(tabella, "^Tabelle/conflicts_mena.csv", row.names = FALSE)

sub_sahara <- fixest::feglm(data = data_2, count~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_growth5,gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(sub_sahara); write.csv(tabella, "^Tabelle/conflicts_sub_sahara.csv", row.names = FALSE)

sud_est_asia <- fixest::feglm(data = data_3, count~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_growth5,gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(sud_est_asia); write.csv(tabella, "^Tabelle/conflicts_sud_est_asia.csv", row.names = FALSE)

cs_america <- fixest::feglm(data = data_4, count~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_growth5,gws_growth10)|region + year, family=quasipoisson)
tabella <- etable(cs_america); write.csv(tabella, "^Tabelle/conflicts_cs_america.csv", row.names = FALSE)






























