suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr)} )

em <- read.csv("^Data/joint/gws_migr_events.csv")
ge <- read.csv("^Data/joint/gws_events.csv")
gm <- read.csv("^Data/joint/gws_migr.csv")

# Rescale GW data (dividing by 1,000)
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

# ANOMALIES
# Create two new variables: mean and std over 1980-2010
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
# Create anomalies for 1, 5, 10 years (averages)
ge <- ge %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_anomalies = (value-mean_region)/std,
         gws_anomalies5 = (gws_avg5-mean_region)/std,
         gws_anomalies10 = (gws_avg10-mean_region)/std)

# TOTAL NUMBER OF CONFLICTS PER YEAR
ge <- ge %>% 
  group_by(year, country, region) %>% 
  mutate(count = sum(conflicts))

write.csv(ge, paste0("^Data/", "gws_events", ".csv"), row.names=FALSE)

#################################################################################################
##### GW-MIGR  ###################################################################################

# GWS AVERAGES 1-5-10 YEARS
gm <- gm %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(gws_avg1 = (lag(value) + value)/2, 
         gws_avg5 = rollmean(value, k = 5, align = "right", fill = NA),
         gws_avg10 = rollmean(value, k = 10, align = "right", fill = NA))

# GWS GROWTH RATE % 1-5-10 YEARS
gm <- gm %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(gws_growth1=((value-lag(value))/lag(value))*100, 
         gws_growth5=((value-lag(value, n=5))/lag(value, n=5))*100,
         gws_growth10=((value-lag(value, n=10))/lag(value, n=10))*100)

# GWS STANDARD DEVIATION 1-5-10 YEARS
gm <- gm %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(gws_std1= rollapply(value, width = 2, FUN = sd, align = "right", fill = NA), 
         gws_std5= rollapply(value, width = 5, FUN = sd, align = "right", fill = NA),
         gws_std10= rollapply(value, width = 10, FUN = sd, align = "right", fill = NA))

# ANOMALIES
# Create two new variables: mean and std over 1980-2010
medie <- gm %>%
  select(year, country, region, value)
medie <- medie %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(mean_region = mean(value))
medie$year <- NULL; medie$value <- NULL
medie <- medie %>%
  distinct(country, region, .keep_all = TRUE)
gm <- left_join(gm,medie,by=c("country","region"))
std_t <- gm %>%
  select(year, country, region, value)
std_t <- std_t %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(std = sd(value))
std_t$year <- NULL; std_t$value <- NULL
std_t <- std_t %>%
  distinct(country, region, .keep_all = TRUE)
gm <- left_join(gm,std_t,by=c("country","region"))
# Create anomalies for 1, 5, 10 years (averages)
gm <- gm %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(gws_anomalies = (value-mean_region)/std,
         gws_anomalies5 = (gws_avg5-mean_region)/std,
         gws_anomalies10 = (gws_avg10-mean_region)/std)

# NUMBER OF MIGRANTS LEAVING A REGION IN THE CONSIDERED INTERVAL DIVIDED BY THE POPULATION
gm <- gm %>%
  mutate(migrants=flow/population)

write.csv(gm, paste0("^Data/", "gws_migr", ".csv"), row.names=FALSE)

#################################################################################################
##### EVENTS-MIGR  ###########################################################################

em$value <- NULL
# NUMBER OF MIGRANTS LEAVING A REGION IN THE CONSIDERED INTERVAL DIVIDED BY THE POPULATION
em <- em %>%
  mutate(migrants=flow/population)

# TOTAL NUMBER OF CONFLICTS PER YEAR
em <- em %>% 
  group_by(year, country, region) %>% 
  mutate(count = sum(conflicts))

# TYPE CONFLICTS AVERAGES 1-5 YEARS
em <- em %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(confl_avg1 = (lag(conflicts) + conflicts) / 2,
         confl_avg5 = rollmean(conflicts, k = 5, align = "right", fill = NA))

# TOTAL CONFLICTS AVERAGES 1-5 YEARS
em <- em %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(count_avg1 = (lag(count) + count) / 2,
         count_avg5 = rollmean(count, k = 5, align = "right", fill = NA))

# TOTAL DEATHS 
em <- em %>% 
  group_by(year, country, region) %>% 
  mutate(all_deaths = sum(deaths))

# Growth rate conflicts 1-5 years
em <- em %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(growth_confl1=((conflicts-lag(conflicts))/lag(conflicts))*100,
         growth_confl5=((conflicts-lag(conflicts, n=5))/lag(conflicts, n=5))*100)

# Growth rate conflicts 1-5 years
em <- em %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(growth_count1=((count-lag(count))/lag(count))*100,
         growth_count5=((count-lag(count, n=5))/lag(count, n=5))*100)


# Save data
write.csv(em, paste0("^Data/", "migr_events", ".csv"), row.names=FALSE)






















