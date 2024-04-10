suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr)} )

ge <- read.csv("^Data/gws_events.csv")
gem <- read.csv("^Data/gws_migr_events_temp.csv")


#################################################################################################
#################################################################################################

# Mean value 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mvalue1 = (lag(value) + value) / 2)
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
# Variation value 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(vvalue1 = value - lag(value))
# Variation conflicts 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(vconfl1 = conflicts - lag(conflicts))
# Vatiation deaths 1-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(vdeaths1 = deaths - lag(deaths))

# Mean value 5-years
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mvalue5 = rollmean(value, k = 5, align = "right", fill = NA))
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
# Variation value 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(sdvalue5 = rollapply(value, width = 5, FUN = sd, align = "right", fill = NA))
# Variation conflicts 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(sdvalue5 = rollapply(conflicts, width = 5, FUN = sd, align = "right", fill = NA))
# Vatiation deaths 5-year
gem <- gem %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(sdvalue5 = rollapply(deaths, width = 5, FUN = sd, align = "right", fill = NA))


# Save data
write.csv(gem, paste0("^Data/", "gws_migr_events", ".csv"), row.names=FALSE)





