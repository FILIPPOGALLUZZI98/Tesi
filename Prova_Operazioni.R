suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr)} )

ge <- read.csv("^Data/gws_events.csv")
gem <- read.csv("^Data/gws_migr_events_temp.csv")


#################################################################################################
#################################################################################################

# Rescale GW data (dividing by 1,000)
gem$value <- gem$value/1000

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

# Growth rate value 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(growth_value1=((value-lag(value))/lag(value))*100)

# Growth rate value 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(growth_value5=((value-lag(value, n=5))/lag(value, n=5))*100)


# Create a new variable with the sum of the conflicts for the same year, country, region for the three types
gem <- gem %>% 
  group_by(year, country, region) %>% 
  mutate(all_confl = sum(conflicts))
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
# Growth rate deaths 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(growth_deaths1=((deaths-lag(deaths))/lag(deaths))*100)
# Mean conflicts 5-years
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mconflict5 = rollmean(conflicts, k = 5, align = "right", fill = NA))
# Mean deaths 5-years
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mdeaths5 = rollmean(deaths, k = 5, align = "right", fill = NA))
# Growth rate conflicts 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(growth_confl5=((all_confl-lag(all_confl, n=5))/lag(all_confl, n=5))*100)
# Growth rate deaths 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate((growth_deaths5=((all_deaths-lag(all_deaths, n=5))/lag(all_deaths, n=5))*100))
# Standard deviation conflicts 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(sconflicts5 = rollapply(conflicts, width = 5, FUN = sd, align = "right", fill = NA))
# Standard deviation deaths 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(sddeaths5 = rollapply(deaths, width = 5, FUN = sd, align = "right", fill = NA))

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

# Save data
write.csv(gem, paste0("^Data/", "gws_migr_events", ".csv"), row.names=FALSE)






















