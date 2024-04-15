suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer)} )

ge <- read.csv("^Data/gws_events.csv")

setFixest_dict(c(log(migrants)="number of migrants/pop in interval", conflicts="number of conflicts per type", value="gws [g/m^3]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies (1980-2010)", count="total number of conflict per year"))
  

events_sum <- subset(ge, type=="state")

model <- fixest::feglm(data=events_sum, count~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
table<- etable(model, tex=TRUE)
write.table(table, file = "^Tables/glm_conflicts.txt", sep = "\t", quote = FALSE, row.names = FALSE)









