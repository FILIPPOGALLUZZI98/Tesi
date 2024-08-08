suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest);library(xtable); library(data.table)} )


#################################################################################################
#################################################################################################

# Upload of the groundwater-migrations dataset
gm <- read.csv("^Data/gws_migr.csv")


plot(gm$n_value,gm$n_migr, cex=0.3, pch=19, xlab="", ylab="", xlim=c(0,0.01))
plot(gm$n_gws_avg5,gm$n_migr, cex=0.3, pch=19, xlab="", ylab="")
plot(gm$n_gws_avg10,gm$n_migr, cex=0.3, pch=19, xlab="", ylab="")
plot(gm$gws_logret5,gm$n_migr, cex=0.3, pch=19, xlab="", ylab="")
plot(gm$gws_logret10,gm$n_migr, cex=0.3, pch=19, xlab="", ylab="", xlim=c(-2,2))
plot(gm$gws_anomalies5,gm$n_migr, cex=0.3, pch=19, xlab="", ylab="", xlim=c(-2,2))
plot(gm$gws_anomalies10,gm$n_migr, cex=0.3, pch=19, xlab="", ylab="")
plot(gm$CV5,gm$n_migr, cex=0.3, pch=19, xlab="", ylab="")
plot(gm$CV10,gm$n_migr, cex=0.3, pch=19, xlab="", ylab="", xlim=c(0,50))

     
