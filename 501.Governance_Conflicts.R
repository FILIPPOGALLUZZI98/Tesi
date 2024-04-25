suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer); library(xtable)} )

ge <- read.csv("^Data/gws_events.csv")
gov <- read.csv("^Data/Governance.csv")

setFixest_dict(c(govern="Governance", conflicts="Type conflicts", value="gws [g/m^3]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies (1980-2010)", count="total conflict"))

gov <- gov[order(gov$govern),]
gov50<-gov[1:50,]; gov100 <- gov[51:100,]; gov150 <- gov[101:150,]; govr <- gov[151:214,]
list50 <- unique(gov50$country); list100 <- unique(gov100$country); list150 <- unique(gov150$country); listr <- unique(govr$country)
events_sum <- subset(ge, type=="state")
set_50 <- events_sum[events_sum$country %in% list50, ]
set_100 <- events_sum[events_sum$country %in% list100, ]
set_150 <- events_sum[events_sum$country %in% list150, ]
set_r <- events_sum[events_sum$country %in% listr, ]

model <- fixest::feglm(data=set_50, count~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
table <- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_50.tex", include.rownames = FALSE)

model <- fixest::feglm(data=set_100, count~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
table <- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_100.tex", include.rownames = FALSE)

model <- fixest::feglm(data=set_150, count~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
table <- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_150.tex", include.rownames = FALSE)

model <- fixest::feglm(data=set_r, count~sw(value,gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
table <- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts_r.tex", include.rownames = FALSE)



