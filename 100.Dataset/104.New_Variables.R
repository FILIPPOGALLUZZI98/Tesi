# In this code the new variables are prepared

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign); library(countrycode);
  library(fixest); library(broom);library(knitr); library(stargazer); library(xtable)} )


#################################################################################################
#################################################################################################
######  DEFINE NEW VARIABLES FOR GW - EVENTS

# Open the dataset
gws_events <- read.csv("^Data/gws_events_j.csv")

# Since the conflict datasets start from 1989 i just need data from 1979
gws_events <- gws_events %>%
  filter(year>1978)
gws_events$orig=NULL; gws_events$deaths=NULL
gws_events <- gws_events %>%
  filter(!is.na(value))  ## Regioni come Antartide in cui non ci sono valori di GWS

# Eliminare le regioni che hanno almeno un anno con pop<2000
data <- subset(gws_events, pop<2000)
nomi <- unique(data$region)
gws_events <- subset(gws_events, !(region %in% nomi))

# GWS PER CAPITA VALUE
gws_events <- gws_events %>% 
  mutate(value_t = value)
gws_events <- gws_events %>% 
  mutate(value = value/pop)

# TOTAL NUMBER OF CONFLICTS PER YEAR
gws_events <- gws_events %>% 
  arrange(year, country, region, type) %>%
  group_by(year, country, region) %>% 
  mutate(count = sum(conflicts))

# NORMALIZATION OF CONLFLITCS
gws_events <- gws_events %>%
  mutate(n_confl = log(1+conflicts))

# NORMALIZATION OF COUNT
gws_events <- gws_events %>%
  mutate(n_count = log(1+count))

# NORMALIZATION OF GWS VALUES (fortemente non simmetrica)
gws_events <- gws_events %>%
  mutate(n_value = log(1+value))

# AVERAGES FOR 1-5-10 YEARS 
#gws_events <- gws_events %>%
#  arrange(year, country, region, type) %>%
#  group_by(country, region, type) %>%
#  mutate(gws_avg1 = (lag(n_value) + n_value)/2, 
#         gws_avg5 = rollmean(n_value, k = 5, align = "right", fill = NA),
#         gws_avg10 = rollmean(n_value, k = 10, align = "right", fill = NA))

# AVERAGES FOR 1-5-10 YEARS (NORMALIZED)
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(n_gws_avg1 = (lag(n_value) + n_value)/2, 
         n_gws_avg5 = rollmean(n_value, k = 5, align = "right", fill = NA),
         n_gws_avg10 = rollmean(n_value, k = 10, align = "right", fill = NA))

# GWS LOGARITHMIC RETURN 1-5-10 YEARS
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_logret=(log(value/(lag(value, n=1)))),
         gws_logret5=(log(value/(lag(value, n=4)))),
         gws_logret10=(log(value/(lag(value, n=9)))))
  
# GWS STANDARD DEVIATION 1-5-10 YEARS
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_std1= rollapply(n_value, width = 2, FUN = sd, align = "right", fill = NA), 
         gws_std5= rollapply(n_value, width = 5, FUN = sd, align = "right", fill = NA),
         gws_std10= rollapply(n_value, width = 10, FUN = sd, align = "right", fill = NA))

# ANOMALIES (1980-2010) 
medie <- gws_events %>%
  select(year, country, region, type, n_value)
medie <- medie %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mean_region = mean(n_value))
medie$year <- NULL; medie$type <- NULL; medie$n_value <- NULL
medie <- medie %>%
  distinct(country, region, .keep_all = TRUE)
gws_events <- left_join(gws_events,medie,by=c("country","region"))
std_t <- gws_events %>%
  select(year, country, region, type, n_value)
std_t <- std_t %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(std = sd(n_value))
std_t$year <- NULL; std_t$type <- NULL; std_t$n_value <- NULL
std_t <- std_t %>%
  distinct(country, region, .keep_all = TRUE)
gws_events <- left_join(gws_events,std_t,by=c("country","region"))
# Create anomalies for 1, 5, 10 years (averages)
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_anomalies = (n_value-mean_region)/std,
         gws_anomalies5 = (n_gws_avg5-mean_region)/std,
         gws_anomalies10 = (n_gws_avg10-mean_region)/std)

# Coefficiente di variazione (%)
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(CV1=(gws_std1/mean_region)*100,
         CV5=(gws_std5/mean_region)*100,
         CV10=(gws_std10/mean_region)*100) 

gws_events <- gws_events %>%
  filter(year>1988)
gws_events$gws_logret[is.nan(gws_events$gws_logret)] <- 0
gws_events$gws_logret5[is.nan(gws_events$gws_logret5)] <- 0
gws_events$gws_logret10[is.nan(gws_events$gws_logret10)] <- 0
gws_events$gws_anomalies[is.nan(gws_events$gws_anomalies)] <- 0
gws_events$gws_anomalies5[is.nan(gws_events$gws_anomalies5)] <- 0
gws_events$gws_anomalies10[is.nan(gws_events$gws_anomalies10)] <- 0
gws_events$CV1[is.nan(gws_events$CV1)] <- 0
gws_events$CV5[is.nan(gws_events$CV5)] <- 0
gws_events$CV10[is.nan(gws_events$CV10)] <- 0


write.csv(gws_events, paste0("^Data/", "gws_events", ".csv"), row.names=FALSE)


#################################################################################################
#################################################################################################
######  DEFINE NEW VARIABLES FOR GW - MIGR

# Open the dataset
gws_migr <- read.csv("^Data/gws_migr_j.csv")
gws_migr$orig=NULL; gws_migr$flow_annual=NULL; gws_migr$worldregion=NULL
gws_migr$outflow_rate_annual=NULL; gws_migr$flow_annual=NULL; gws_migr$year_cat10=NULL
gws_migr <- gws_migr %>%
  filter(!is.na(value))  ## Regioni come Antartide in cui non ci sono valori di GWS

# Selezioni solo i paesi che contengono valori per migrazioni
data <- subset(gws_migr, flow>0)
data <- data %>%
  filter(!is.na(flow))
nomi <- unique(data$region)
gws_migr <- gws_migr %>%
  filter(region %in% nomi)

# NUMBER OF MIGRANTS LEAVING A REGION IN THE CONSIDERED INTERVAL DIVIDED BY THE POPULATION
# PERCENTAGE OF TOTAL POPULATION
gws_migr <- gws_migr %>%
  mutate(migrants=(flow/pop))

# GWS PER CAPITA VALUE
gws_migr <- gws_migr %>% 
  mutate(value_t = value)
gws_migr <- gws_migr %>% 
  mutate(value = value/pop)

# NORMALIZATION OF VALUE
# NORMALIZATION OF GWS VALUES (fortemente non simmetrica)
gws_migr <- gws_migr %>%
  mutate(n_value = log(1+value))

# NORMALIZATION OF MIGRANTS
gws_migr <- gws_migr %>%
  mutate(n_migr = log(1+migrants))

# GWS AVERAGES 1-5-10 YEARS
#gws_migr <- gws_migr %>%
#  arrange(year, country, region) %>%
#  group_by(country, region) %>%
#  mutate(gws_avg1 = (lag(value) + value)/2, 
#         gws_avg5 = rollmean(value, k = 5, align = "right", fill = NA),
#         gws_avg10 = rollmean(value, k = 10, align = "right", fill = NA))

# AVERAGES FOR 1-5-10 YEARS (NORMALIZED)
gws_migr <- gws_migr %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(n_gws_avg1 = (lag(n_value) + n_value)/2, 
         n_gws_avg5 = rollmean(n_value, k = 5, align = "right", fill = NA),
         n_gws_avg10 = rollmean(n_value, k = 10, align = "right", fill = NA))

# GWS LOGARITHMIC RETURN % 1-5-10 YEARS
gws_migr <- gws_migr %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(gws_logret=(log(value/(lag(value, n=1)))),
         gws_logret5=(log(value/(lag(value, n=4)))),
         gws_logret10=(log(value/(lag(value, n=9)))))

# GWS STANDARD DEVIATION 1-5-10 YEARS
gws_migr <- gws_migr %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(gws_std1= rollapply(n_value, width = 2, FUN = sd, align = "right", fill = NA), 
         gws_std5= rollapply(n_value, width = 5, FUN = sd, align = "right", fill = NA),
         gws_std10= rollapply(n_value, width = 10, FUN = sd, align = "right", fill = NA))

# ANOMALIES
# Create two new variables: mean and std over 1980-2010
medie <- gws_migr %>%
  select(year, country, region, n_value)
medie <- medie %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(mean_region = mean(n_value))
medie$year <- NULL; medie$n_value <- NULL
medie <- medie %>%
  distinct(country, region, .keep_all = TRUE)
gws_migr <- left_join(gws_migr,medie,by=c("country","region"))
std_t <- gws_migr %>%
  select(year, country, region, n_value)
std_t <- std_t %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(std = sd(n_value))
std_t$year <- NULL; std_t$n_value <- NULL
std_t <- std_t %>%
  distinct(country, region, .keep_all = TRUE)
gws_migr <- left_join(gws_migr,std_t,by=c("country","region"))
# Create anomalies for 1, 5, 10 years (averages)
gws_migr <- gws_migr %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(gws_anomalies = (n_value-mean_region)/std,
         gws_anomalies5 = (n_gws_avg5-mean_region)/std,
         gws_anomalies10 = (n_gws_avg10-mean_region)/std)

# Coefficiente di variazione (%)
gws_migr <- gws_migr %>%
  arrange(year, country, region) %>%
  group_by(country, region) %>%
  mutate(CV1=(gws_std1/mean_region)*100,
         CV5=(gws_std5/mean_region)*100,
         CV10=(gws_std10/mean_region)*100) 

# Rimozione valori inutili
gws_migr <- gws_migr %>%
  filter(!is.na(population))
gws_migr <- gws_migr %>%
  filter(!is.na(CV10))
# Rimuovere regioni con flow>pop
gws_migr <- subset(gws_migr, n_migr<0.6)

write.csv(gws_migr, paste0("^Data/", "gws_migr", ".csv"), row.names=FALSE)



