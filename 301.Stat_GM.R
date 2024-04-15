suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer)} )

gm <- read.csv("^Data/gws_migr.csv")

setFixest_dict(c(migrants="number of migrants/population in the interval", value="groundwater storage [g/m^3]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies (1980-2010)"))


data_1 <- subset(gm, interval==1)
model <- fixest::feols(data=data_1, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table<- etable(model, tex=TRUE)
write.table(table, file = "^Tables/glm_migration_1.txt", sep = "\t", quote = FALSE, row.names = FALSE)

data_5 <- subset(gm, interval==5)
model <- fixest::feols(data=data_1, log(migrants)~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year)
table<- etable(model, tex=TRUE)
write.table(table, file = "^Tables/glm_migration_5.txt", sep = "\t", quote = FALSE, row.names = FALSE)




