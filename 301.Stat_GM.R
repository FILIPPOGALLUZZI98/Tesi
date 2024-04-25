suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer); library(xtable)} )

gm <- read.csv("^Data/gws_migr.csv")

setFixest_dict(c(migrants="number of migrants/pop in interval", value="gws [g/m^3]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies 1y (1980-2010)", gws_anomalies="gws anomalies 5y (1980-2010)",
                 gws_anomalies="gws anomalies 10y (1980-2010)"))


data_1 <- subset(gm, interval==1)
model <- fixest::feglm(data=data_1, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
table<- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_migration_1.tex", include.rownames = FALSE)

data_5 <- subset(gm, interval==5)
model <- fixest::feglm(data=data_5, log(migrants)~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=gaussian)
table<- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_migration_5.tex", include.rownames = FALSE)




