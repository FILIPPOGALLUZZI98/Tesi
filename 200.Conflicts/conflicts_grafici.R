suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest);library(xtable); library(data.table)} )


#################################################################################################
#################################################################################################

# Upload of the groundwater-migrations dataset
ge <- read.csv("^Data/gws_events.csv")

par(mfrow=c(2,2))
plot(ge$n_value,log(ge$count+1), cex=0.3, pch=19, ylab="Conflicts",xlab="GWS/Population",xlim=c(0,0.02))
plot(ge$n_gws_avg5,log(ge$count+1), cex=0.3, pch=19, ylab="Conflicts",xlab="GWS/Population 5-y",xlim=c(0,0.02))
plot(ge$n_gws_avg10,log(ge$count+1), cex=0.3, pch=19, ylab="Conflicts",xlab="GWS/Population 10-y",xlim=c(0,0.02))

par(mfrow=c(2,2))
plot(ge$gws_logret,log(ge$count+1), cex=0.3, pch=19, ylab="Conflicts",xlab="Logarithmic Return 1-y",xlim=c(-2,2))
plot(ge$gws_logret5,log(ge$count+1), cex=0.3, pch=19, ylab="Conflicts",xlab="Logarithmic Return 5-y",xlim=c(-5,5))
plot(ge$gws_logret10,log(ge$count+1), cex=0.3, pch=19,ylab="Conflicts",xlab="Logarithmic Return 10-y",xlim=c(-5,5))

par(mfrow=c(2,2))
plot(ge$gws_anomalies,log(ge$count+1), cex=0.3, pch=19, ylab="Conflicts",xlab="Anomalies 1-y (1980-2010",xlim=c(-4,4))
plot(ge$gws_anomalies5,log(ge$count+1), cex=0.3, pch=19, ylab="Conflicts",xlab="Anomalies 5-y (1980-2010",xlim=c(-4,4))
plot(ge$gws_anomalies10,log(ge$count+1), cex=0.3, pch=19,ylab="Conflicts",xlab="Anomalies 10-y (1980-2010",xlim=c(-4,4))

par(mfrow=c(2,2))
plot(ge$CV1,log(ge$count+1), cex=0.3, pch=19, ylab="Conflicts",xlab="Coefficient of Variation 1-y",xlim=c(0,100))
plot(ge$CV5,log(ge$count+1), cex=0.3, pch=19, ylab="Conflicts",xlab="Coefficient of Variation 5-y",xlim=c(0,100))
plot(ge$CV10,log(ge$count+1), cex=0.3, pch=19, ylab="Conflicts",xlab="Coefficient of Variation 10-y",xlim=c(0,100))








