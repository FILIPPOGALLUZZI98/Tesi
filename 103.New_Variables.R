suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr)} )

gem <- read.csv("^Data/gws_migr_events_temp.csv")
ge <- read.csv("^Data/gws_events.csv")
gm <- read.csv("^Data/gws_migr.csv")

# Rescale GW data (dividing by 1,000)
gem$value <- gem$value/1000
ge$value <- ge$value/1000
gm$value <- gm$value/1000

#################################################################################################
####  GW-EVENTS #################################################################################

# GWS AVERAGES 1-5-10 YEARS
ge <- ge %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_avg1 = (lag(value) + value)/2, 
         gws_avg5 = rollmean(value, k = 5, align = "right", fill = NA),
         gws_avg10 = rollmean(value, k = 10, align = "right", fill = NA))

# GWS GROWTH RATE % 1-5-10 YEARS
ge <- ge %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_growth1=((value-lag(value))/lag(value))*100, 
         gws_growth5=((value-lag(value, n=5))/lag(value, n=5))*100,
         gws_growth10=((value-lag(value, n=10))/lag(value, n=10))*100)

# GWS STANDARD DEVIATION 1-5-10 YEARS
ge <- ge %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_std1= rollapply(value, width = 2, FUN = sd, align = "right", fill = NA), 
         gws_std5= rollapply(value, width = 5, FUN = sd, align = "right", fill = NA),
         gws_std10= rollapply(value, width = 10, FUN = sd, align = "right", fill = NA))

# GWS ANOMALIES
medie <- ge %>%
  select(year, country, region, type, value)
medie <- medie %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mean_region = mean(value))
medie$year <- NULL; medie$type <- NULL; medie$value <- NULL
medie <- medie %>%
  distinct(country, region, .keep_all = TRUE)
ge <- left_join(ge,medie,by=c("country","region"))
std_t <- ge %>%
  select(year, country, region, type, value)
std_t <- std_t %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(std = sd(value))
std_t$year <- NULL; std_t$type <- NULL; std_t$value <- NULL
std_t <- std_t %>%
  distinct(country, region, .keep_all = TRUE)
ge <- left_join(ge,std_t,by=c("country","region"))
ge <- ge %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_anomalies = (value-mean_region)/std)

# CONFLICTS AVERAGES 1-5-10 YEARS
ge <- ge %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(confl_avg1 = (lag(conflicts) + conflicts) / 2,
         confl_avg5 = rollmean(conflicts, k = 5, align = "right", fill = NA),
         confl_avg10 = rollmean(conflicts, k = 10, align = "right", fill = NA))

ge <- ge %>% 
  group_by(year, country, region) %>% 
  mutate(count = sum(conflicts))
ge <- ge %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(count_avg1 = (lag(count) + count) / 2,
         count_avg5 = rollmean(count, k = 5, align = "right", fill = NA),
         count_avg10 = rollmean(count, k = 10, align = "right", fill = NA))


#################################################################################################
##### GW-EVENTS-MIGR  ###########################################################################

# Mean value 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mvalue1 = (lag(value) + value)/2)

# Variation value 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(vvalue1 = value - lag(value))

# Growth rate value 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(growth_value1=((value-lag(value))/lag(value))*100)


# Mean value 5-years
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mvalue5 = rollmean(value, k = 5, align = "right", fill = NA))

# Standard Deviation value 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(sdvalue5 = rollapply(value, width = 5, FUN = sd, align = "right", fill = NA))

# Growth rate value 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(growth_value5=((value-lag(value, n=5))/lag(value, n=5))*100)


# Anomalies 
medie <- gem %>%
  select(year, country, region, type, value)
medie <- medie %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mean_region = mean(value))
medie$year <- NULL; medie$type <- NULL; medie$value <- NULL
medie <- medie %>%
  distinct(country, region, .keep_all = TRUE)
gem <- left_join(gem,medie,by=c("country","region"))
std_t <- gem %>%
  select(year, country, region, type, value)
std_t <- std_t %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(std = sd(value))
std_t$year <- NULL; std_t$type <- NULL; std_t$value <- NULL
std_t <- std_t %>%
  distinct(country, region, .keep_all = TRUE)
gem <- left_join(gem,std_t,by=c("country","region"))
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(anomaly_it = (value-mean_region)/std)



# Create a new variable with the sum of the conflicts for the same year, country, region for the three types
gem <- gem %>% 
  group_by(year, country, region) %>% 
  mutate(count = sum(conflicts))
gem <- gem %>% 
  group_by(year, country, region) %>% 
  mutate(all_deaths = sum(deaths))

# Mean conflicts 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mconfl1 = (lag(conflicts) + conflicts) / 2)

# Mean deaths 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mdeaths1 = (lag(deaths) + deaths) / 2)

# Growth rate conflicts 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(growth_confl1=((conflicts-lag(conflicts))/lag(conflicts))*100)

# Mean conflicts 5-years
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mconflict5 = rollmean(conflicts, k = 5, align = "right", fill = NA))

# Growth rate conflicts 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(growth_confl5=((all_confl-lag(all_confl, n=5))/lag(all_confl, n=5))*100)

# Standard deviation conflicts 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(sconflicts5 = rollapply(conflicts, width = 5, FUN = sd, align = "right", fill = NA))


# Save data
write.csv(gem, paste0("^Data/", "gws_migr_events", ".csv"), row.names=FALSE)






















