######  DEFINE NEW VARIABLES FOR GW - EVENTS

# Open the dataset
gws_events <- read.csv("^Data/joint/gws_events.csv")
gws_events$value <- gws_events$value*1000
gws_events <- gws_events %>%
  filter(year>1978)

# TOTAL NUMBER OF CONFLICTS PER YEAR
gws_events <- gws_events %>% 
  arrange(year, country, region, type) %>%
  group_by(year, country, region) %>% 
  mutate(count = sum(conflicts))

# NORMALIZATION OF GWS VALUES
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country,region) %>%
  mutate(z_value = (value - min(value)) / (max(value)-min(value) ))

# NORMALIZATION OF EVENTS
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country,region) %>%
  mutate(z_count = (count - min(count)) / (max(count)-min(count)))

# NORMALIZATION OF CONLFLITCS
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country,region) %>%
  mutate(z_confl = (conflicts - min(conflicts)) / (max(conflicts)-min(conflicts)))


# CHANGE NAMES
gws_events$value <- gws_events$z_value; gws_events$z_value=NULL
gws_events$count <- gws_events$z_count; gws_events$z_count=NULL
gws_events$conflicts <- gws_events$z_confl; gws_events$z_confl=NULL


# AVERAGES FOR 1-5-10 YEARS 
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_avg1 = (lag(value) + value)/2, 
         gws_avg5 = rollmean(value, k = 5, align = "right", fill = NA),
         gws_avg10 = rollmean(value, k = 10, align = "right", fill = NA))

# GWS GROWTH RATE % 1-5-10 YEARS
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_growth1=((value-lag(value))/lag(value))*100, 
         gws_growth5=((value-lag(value, n=5))/lag(value, n=5))*100,
         gws_growth10=((value-lag(value, n=10))/lag(value, n=10))*100)

# GWS STANDARD DEVIATION 1-5-10 YEARS
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_std1= rollapply(value, width = 2, FUN = sd, align = "right", fill = NA), 
         gws_std5= rollapply(value, width = 5, FUN = sd, align = "right", fill = NA),
         gws_std10= rollapply(value, width = 10, FUN = sd, align = "right", fill = NA))

# ANOMALIES (1980-2010) 
medie <- gws_events %>%
  select(year, country, region, type, value)
medie <- medie %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(mean_region = mean(value))
medie$year <- NULL; medie$type <- NULL; medie$value <- NULL
medie <- medie %>%
  distinct(country, region, .keep_all = TRUE)
gws_events <- left_join(gws_events,medie,by=c("country","region"))
std_t <- gws_events %>%
  select(year, country, region, type, value)
std_t <- std_t %>%
  filter(year >= 1980 & year <= 2010) %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(std = sd(value))
std_t$year <- NULL; std_t$type <- NULL; std_t$value <- NULL
std_t <- std_t %>%
  distinct(country, region, .keep_all = TRUE)
gws_events <- left_join(gws_events,std_t,by=c("country","region"))
# Create anomalies for 1, 5, 10 years (averages)
gws_events <- gws_events %>%
  arrange(year, country, region, type) %>%
  group_by(country, region, type) %>%
  mutate(gws_anomalies = (value-mean_region)/std,
         gws_anomalies5 = (gws_avg5-mean_region)/std,
         gws_anomalies10 = (gws_avg10-mean_region)/std)


write.csv(gws_events, paste0("^Data/", "gws_events_normalized", ".csv"), row.names=FALSE)


####################################################################################################
####################################################################################################
####  STATISTICAL ANALYSIS

ge <- read.csv("^Data/gws_events_normalized.csv")


# Setting of the dictionary for the tables
setFixest_dict(c(conflicts="# conflicts", value="gws [Kg/m^2]",
                 gws_avg1="gws 1-y", gws_avg5="gws 5-y", gws_avg10="gws 10-y",
                 gws_growth1="gws growth rate 1-y", gws_growth5="gws growth rate 5-y", gws_growth10="gws growth rate 10-y",
                 gws_std1="gws st dev 1-y", gws_std5="gws st dev 5-y", gws_std10="gws st dev 10-y",
                 gws_anomalies="gws anomalies 1y (1980-2010)", gws_anomalies5="gws anomalies 5y (1980-2010)",
                 gws_anomalies10="gws anomalies 10y (1980-2010)",count="# conflict"))


#################################################################################################
#################################################################################################
####  GENERALIZED LINEAR REGRESSION FOR GLOBAL DATA

# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
events_sum <- subset(ge, type=="state" & year>1988)

model <- fixest::feglm(data=events_sum, count~sw(gws_avg1,gws_avg5,gws_avg10, gws_anomalies, gws_anomalies5, gws_anomalies10, gws_std1, gws_std5,gws_std10, gws_growth1, gws_growth5, gws_growth10)|region + year, family=quasipoisson)
etable(model)










