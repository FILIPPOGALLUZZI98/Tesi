suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr)} )

em <-read.csv("^Data/migr_events.csv")


setFixest_dict(c(conflicts="Type conflicts",count="Total Conflicts",confl_avg1="Avg Type Confl 1",confl_avg5="Avg Type Confl 5",count_avg1="Avg Total Confl 1",
                 count_avg5="Avg Total Confl 5", growth_count1="Growth % Total Confl 1", growth_count5="Growth % Total Confl 5",
                 growth_confl1="Growth % Type Confl 1", growth_confl5="Growth % Type Confl 5"))


data_1 <- subset(em, interval==1)
model <- fixest::feglm(data=data_1, log(migrants)~sw(count,count_avg1,count_avg5,growth_count1)|region + year, family=gaussian)
table<- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_migr_confl1.tex", include.rownames = FALSE)

data_5 <- subset(em, interval==5)
model <- fixest::feglm(data=data_5, log(migrants)~sw(count,count_avg1,count_avg5,growth_count1,growth_count5)|region + year, family=gaussian)
table<- xtable(etable(model), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_migr_confl5.tex", include.rownames = FALSE)






















