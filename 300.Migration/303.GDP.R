# In this code statistical analysis is performed for GWS and Migration data divided by GDP 
# Generalized linear regression with fixed effects (region and year)
# Family: gaussian

#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
gm <- read.csv("^Data/gws_migr.csv")
gm_1 <- subset(gm, interval==1)
gm_5 <- subset(gm, interval==5)

# Select the GDP list for one year (2005)
gdp_data <- WDI(indicator = "NY.GDP.MKTP.PP.KD", start = 2005, end = 2005, extra = TRUE)
gdp_data <- subset(gdp_data, year== 2005)

# Divide the countries into four categories
gdp_high <- subset(gdp_data, income == "High income"); name_high <- unique(gdp_high$country)
gdp_low <- subset(gdp_data, income == "Low income"); name_low <- unique(gdp_low$country)
gdp_lowmid <- subset(gdp_data, income == "Lower middle income"); name_lowmid <- unique(gdp_lowmid$country)
gdp_highmid <- subset(gdp_data, income == "Upper middle income"); name_highmid <- unique(gdp_highmid$country)



#### HIGH GDP
gm_high <- subset(gm_1, country %in% name_high)
high1 <- fixest::feglm(data=gm_high, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(high1)
gm_high <- subset(gm_5, country %in% name_high)
high5 <- fixest::feglm(data=gm_high, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(high5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_highGDP.csv", row.names = FALSE)


#### LOW GDP
gm_low <- subset(gm_1, country %in% name_low)
low1 <- fixest::feglm(data=gm_low, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(low1)
gm_low <- subset(gm_5, country %in% name_low)
low5 <- fixest::feglm(data=gm_low, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(low5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_lowGDP.csv", row.names = FALSE)


#### MID-HIGH GPD
gm_highmid <- subset(gm_1, country %in% name_highmid)
midhigh1 <- fixest::feglm(data=gm_highmid, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(midhigh1)
gm_highmid <- subset(gm_5, country %in% name_highmid)
midhigh5 <- fixest::feglm(data=gm_highmid, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(midhigh5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_midhighGDP.csv", row.names = FALSE)


#### LOW-MID GDP
gm_lowmid <- subset(gm_1, country %in% name_lowmid)
lowmid1 <- fixest::feglm(data=gm_lowmid, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(lowmid1)
gm_lowmid <- subset(gm_5, country %in% name_lowmid)
lowmid5 <- fixest::feglm(data=gm_lowmid, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(lowmid5)
tabella <- migr_tabella(tabella1, tabella5)
write.csv(tabella, "^Tabelle/migration_lowmidGDP.csv", row.names = FALSE)








