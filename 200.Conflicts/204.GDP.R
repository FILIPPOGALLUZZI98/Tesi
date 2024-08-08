# In this code statistical analysis is performed for GWS and Conflicts data divided by GDP
# Generalized linear regression with fixed effects (region and year)
# Family: quasipoisson

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest);library(xtable); library(data.table);library(WDI)} )


#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")

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

# Select the GDP list for one year (2019)
gdp_data <- WDI(indicator = "NY.GDP.MKTP.PP.KD", start = 1988, end = 2022, extra = TRUE)
gdp_data <- subset(gdp_data, year== 2019)

# Divide the countries into four categories
gdp_high <- subset(gdp_data, income == "High income"); name_high <- unique(gdp_high$country)
gdp_low <- subset(gdp_data, income == "Low income"); name_low <- unique(gdp_low$country)
gdp_lowmid <- subset(gdp_data, income == "Lower middle income"); name_lowmid <- unique(gdp_lowmid$country)
gdp_highmid <- subset(gdp_data, income == "Upper middle income"); name_highmid <- unique(gdp_highmid$country)

# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
ge <- subset(ge, type=="state")


# Statistical model and tables

ge_high <- subset(ge, country %in% name_high)
high <- fixest::feglm(data=ge_high, count~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(high); write.csv(tabella, "^Tabelle/conflicts_highGDP.csv", row.names = FALSE)

ge_low <- subset(ge, country %in% name_low)
low <- fixest::feglm(data=ge_low, count~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(low); write.csv(tabella, "^Tabelle/conflicts_lowGDP.csv", row.names = FALSE)

ge_highmid <- subset(ge, country %in% name_highmid)
midhigh <- fixest::feglm(data=ge_highmid, count~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(midhigh); write.csv(tabella, "^Tabelle/conflicts_midhighGDP.csv", row.names = FALSE)

ge_lowmid <- subset(ge, country %in% name_lowmid)
lowmid <- fixest::feglm(data=ge_lowmid, count~sw(n_value,n_gws_avg5,n_gws_avg10, gws_anomalies5, gws_anomalies10, CV5, CV10, gws_logret5,gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(lowmid); write.csv(tabella, "^Tabelle/conflicts_lowmidGDP.csv", row.names = FALSE)



