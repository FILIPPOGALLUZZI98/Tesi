suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer)} )

gm <- read.csv("^Data/gws_migr.csv")

setFixest_dict(c(conflicts="number of conflicts per type", value="groundwater storage [g/m^3]",
                 gws_avg1="groundwater storage average 1-y", gws_avg5="groundwater storage average 5-y", gws_avg10="groundwater storage average 10-y",
                 gws_growth1="groundwater growth rate 1-y", gws_growth5="groundwater growth rate 5-y", gws_growth10="groundwater growth rate 10-y",
                 gws_std1="groundwater standard deviation 1-y", gws_std5="groundwater standard deviation 5-y", gws_std10="groundwater standard deviation 10-y",
                 gws_anomalies="groundwater anomalies (1980-2010)", count="total number of conflict per year"))


events_sum <- subset(ge, type=="state")








ols_v <- fixest::feols(data=events_sum, log(1+all_confl)~value|region + year)




