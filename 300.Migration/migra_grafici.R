suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest);library(xtable); library(data.table)} )


#################################################################################################
#################################################################################################

# Upload of the groundwater-migrations dataset
gm <- read.csv("^Data/gws_migr.csv")

par(mfrow=c(2,2))
plot(gm$n_value,gm$n_migr, cex=0.3, pch=19, ylab="Migrants/Population",xlab="GWS/Population",xlim=c(0,0.02))
plot(gm$n_gws_avg5,gm$n_migr, cex=0.3, pch=19, ylab="Migrants/Population",xlab="GWS/Population",xlim=c(0,0.02))
plot(gm$n_gws_avg10,gm$n_migr, cex=0.3, pch=19, ylab="Migrants/Population",xlab="GWS/Population",xlim=c(0,0.02))

par(mfrow=c(2,2))
plot(gm$gws_logret,gm$n_migr, cex=0.3, pch=19, ylab="Migrants/Population",xlab="Logarithmic Return 1-y",xlim=c(-2,2))
plot(gm$gws_logret5,gm$n_migr, cex=0.3, pch=19, ylab="Migrants/Population",xlab="Logarithmic Return 5-y",xlim=c(-5,5))
plot(gm$gws_logret10,gm$n_migr, cex=0.3, pch=19,ylab="Migrants/Population",xlab="Logarithmic Return 10-y",xlim=c(-5,5))

par(mfrow=c(2,2))
plot(gm$gws_anomalies,gm$n_migr, cex=0.3, pch=19, ylab="Migrants/Population",xlab="Anomalies 1-y (1980-2010",xlim=c(-4,4))
plot(gm$gws_anomalies5,gm$n_migr, cex=0.3, pch=19, ylab="Migrants/Population",xlab="Anomalies 5-y (1980-2010",xlim=c(-4,4))
plot(gm$gws_anomalies10,gm$n_migr, cex=0.3, pch=19,ylab="Migrants/Population",xlab="Anomalies 10-y (1980-2010",xlim=c(-4,4))

par(mfrow=c(2,2))
plot(gm$CV1,gm$n_migr, cex=0.3, pch=19, ylab="Migrants/Population",xlab="Coefficient of Variation 1-y",xlim=c(0,100))
plot(gm$CV5,gm$n_migr, cex=0.3, pch=19, ylab="Migrants/Population",xlab="Coefficient of Variation 5-y",xlim=c(0,100))
plot(gm$CV10,gm$n_migr, cex=0.3, pch=19, ylab="Migrants/Population",xlab="Coefficient of Variation 10-y",xlim=c(0,100))







     
