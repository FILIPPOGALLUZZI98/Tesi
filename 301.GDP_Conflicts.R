# In this code statistical analysis is performed for GWS and Conflicts data

# Fixed effects: year and region; Family: Quasipoisson
# Independent variables: gws (average 1-5-10 y), gws anomalies (average 1-5-10 y), gws standard deviations (1-5-10 y)
# gws growth rate percentage (1-5-10 y)
# Dependent variable: count (number of conflicts of all types, per each year and region)


#################################################################################################
#################################################################################################
###### PACKAGES

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer); library(xtable); library(WDI)} )


#################################################################################################
#################################################################################################

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")

# Setting of the dictionary for the tables
setFixest_dict(c(conflicts="Type conflicts", value="gws [g/m^2]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies 1y (1980-2010)", gws_anomalies5="gws anomalies 5y (1980-2010)",
                 gws_anomalies10="gws anomalies 10y (1980-2010)",count="total conflict"))


#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR GLOBAL DATA

gdp_data <- WDI(indicator = "NY.GDP.MKTP.PP.KD", start = 1988, end = 2022, extra = TRUE)
gdp_data <- subset(gdp_data, year== 2019)

gdp_high <- subset(gdp_data, income == "High income"); name_high <- unique(gdp_high$country)
gdp_low <- subset(gdp_data, income == "Low income"); name_low <- unique(gdp_low$country)
gdp_lowmid <- subset(gdp_data, income == "Lower middle income"); name_lowmid <- unique(gdp_lowmid$country)
gdp_highmid <- subset(gdp_data, income == "Upper middle income"); name_highmid <- unique(gdp_highmid$country)

ge_high <- subset(ge, country %in% name_high)
ge_low <- subset(ge, country %in% name_low)
ge_highmid <- subset(ge, country %in% name_highmid)
ge_lowmid <- subset(ge, country %in% name_lowmid)


high <- fixest::feglm(data=ge_high, conflicts~sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table_state<- xtable(etable(high), tex=TRUE)
print(table_state, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/GDPhigh_confl.tex", include.rownames = FALSE)

low <- fixest::feglm(data=ge_low, conflicts~sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table_state<- xtable(etable(low), tex=TRUE)
print(table_state, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/GDPlow_confl.tex", include.rownames = FALSE)

midhigh <- fixest::feglm(data=ge_highmid, conflicts~sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table_state<- xtable(etable(midhigh), tex=TRUE)
print(table_state, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/GDPhighmid_confl.tex", include.rownames = FALSE)

lowmid <- fixest::feglm(data=ge_lowmid, conflicts~sw(gws_avg1,gws_avg5,gws_avg10,gws_anomalies,gws_anomalies5,gws_anomalies10,gws_std1,gws_std5,gws_std10,gws_growth1,gws_growth5,gws_growth10)|region + year, family=quasipoisson)
table_state<- xtable(etable(lowmid), tex=TRUE)
print(table_state, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/GDPlowmid_confl.tex", include.rownames = FALSE)




