suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr)} )

em <-read.csv("^Data/migr_events.csv")


setFixest_dict(c(conflicts="Type conflicts",count="Total Conflicts",confl_avg1="Avg Type Confl 1",confl_avg5="Avg Type Confl 5",count_avg1="Avg Total Confl 1",
                 count_avg5="Avg Total Confl 5", growth_count1="Growth % Total Confl 1", growth_count5="Growth % Total Confl 5"))


events_sum <- subset(ge, type=="state")

model <- fixest::feglm(data=events_sum, count~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
table <- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_conflicts.tex", include.rownames = FALSE)



















