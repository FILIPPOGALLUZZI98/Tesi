#################################################################################################
####  CONFLICTS ANALYSIS  ####
#################################################################################################

# Upload of the groundwater-events dataset
ge <- read.csv("^Data/gws_events.csv")
# Create a subset of the dataset (because the variables are counted thrice (one for each type of conflict)
events_sum <- subset(ge, type=="state")

## Global data
model <- fixest::feglm(data=events_sum, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(model)

## GEOGRAPHIC SUBDIVISION
continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Africa <- fixest::feglm(data = data_continent, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(Africa)
continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Asia <- fixest::feglm(data = data_continent, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(Asia)
continent <- "Oceania"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Oceania <- fixest::feglm(data = data_continent, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(Oceania)
continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- events_sum %>%
  filter(get_continent(country) == continent)
Europe <- fixest::feglm(data = data_continent, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(Europe)
North_America <- c("United States", "Canada", "Mexico")
N_A <- events_sum[events_sum$country %in% North_America, ]
Namerica <- fixest::feglm(data = N_A, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(Namerica)
South_America <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- events_sum[events_sum$country %in% South_America, ]
Samerica <- fixest::feglm(data = S_A, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(Samerica)
Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Jamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- events_sum[events_sum$country %in% Central_America, ]
Camerica <- fixest::feglm(data = C_A, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(Camerica)

## LIST OF NAMES OF COUNTRIES FOR: MENA, SUB-SAHARAN AFRICA, SOUTH-EAST ASIA, CENTRAL AND SOUTH AMERICA
lista_1 <- sahel_mena_countries <- c(
  "Algeria", "Bahrain", "Chad", "Djibouti", "Egypt", "Eritrea", "Iran", "Iraq", 
  "Israel", "Jordan", "Kuwait", "Lebanon", "Libya", "Mauritania", "Morocco", 
  "Oman", "Qatar", "Saudi Arabia", "Somalia", "Sudan", "Syria", "Tunisia", 
  "United Arab Emirates", "Yemen", "Western Sahara")
data_1 <- events_sum[events_sum$country %in% lista_1, ]
lista_2 <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", 
             "Cameroon", "Central African Republic", "Comoros", "Congo", 
             "Democratic Republic of the Congo", "Equatorial Guinea", "Eswatini", 
             "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
             "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", 
             "Mali", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", 
             "São Tomé and Príncipe", "Senegal", "Seychelles", "Sierra Leone", 
             "South Africa", "South Sudan", "Tanzania", "Togo", "Uganda", 
             "Zambia", "Zimbabwe")
data_2 <- events_sum[events_sum$country %in% lista_2, ]
lista_3 <- c("Brunei", "Cambodia", "East Timor", "Indonesia", "Laos", 
             "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", 
             "Vietnam")
data_3 <- events_sum[events_sum$country %in% lista_3, ]
lista_4 <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", 
             "Nicaragua", "Panama", "Argentina", "Bolivia", "Brazil", 
             "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", 
             "Peru", "Suriname", "Uruguay", "Venezuela")
data_4 <- events_sum[events_sum$country %in% lista_4, ]


mena <- fixest::feglm(data = data_1, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(mena)
sub_sahara <- fixest::feglm(data = data_2, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(sub_sahara)
sud_est_asia <- fixest::feglm(data = data_3, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(sud_est_asia)
cs_america <- fixest::feglm(data = data_4, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(cs_america)


## INCOME
# Select the GDP list for one year (2005)
gdp_data <- WDI(indicator = "NY.GDP.MKTP.PP.KD", start = 2005, end = 2005, extra = TRUE)
gdp_data <- subset(gdp_data, year== 2005)
# Divide the countries into four categories
gdp_high <- subset(gdp_data, income == "High income"); name_high <- unique(gdp_high$country)
gdp_low <- subset(gdp_data, income == "Low income"); name_low <- unique(gdp_low$country)
gdp_lowmid <- subset(gdp_data, income == "Lower middle income"); name_lowmid <- unique(gdp_lowmid$country)
gdp_highmid <- subset(gdp_data, income == "Upper middle income"); name_highmid <- unique(gdp_highmid$country)

ge_high <- subset(events_sum, country %in% name_high)
high <- fixest::feglm(data=ge_high, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(high)
ge_low <- subset(events_sum, country %in% name_low)
low <- fixest::feglm(data=ge_low, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(low)
ge_highmid <- subset(events_sum, country %in% name_highmid)
midhigh <- fixest::feglm(data=ge_highmid, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(midhigh)
ge_lowmid <- subset(events_sum, country %in% name_lowmid)
lowmid <- fixest::feglm(data=ge_lowmid, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(lowmid)


## GOVERNANCE
gov <- read.csv("^Data/Govern.csv")
# Select the governance list 
gov <- head(gov, -5)
gov <- gov[, -c(1, 2,4)]
names(gov) <- c("country", "govern")
gov$govern <- as.numeric(gov$govern)
gov <- gov %>%
  arrange(desc(govern))
gov <- na.omit(gov)

# Create 3 classes for high, medium and low governance
gov1 <- gov[1:71, ]; name_gov1 <- unique(gov1$country)  ## high
gov2 <- gov[72:142, ]; name_gov2 <- unique(gov2$country)  ## medium
gov3 <- gov[143:213, ]; name_gov3 <- unique(gov3$country)   ## low

ge_gov1 <- subset(events_sum, country %in% name_gov1)
gov1 <- fixest::feglm(data=ge_gov1, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(gov1)
ge_gov2 <- subset(events_sum, country %in% name_gov2)
gov2 <- fixest::feglm(data=ge_gov2, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(gov2)
ge_gov3 <- subset(events_sum, country %in% name_gov3)
gov3 <- fixest::feglm(data=ge_gov3, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(gov3)


## PET
pet <- read.csv("^Data/pet.csv")
# Create 2 classes for high and low PET
pet_l <- pet[1:140, ]; name_pet_l <- unique(pet_l$country)  ## low
pet_h <- pet[141:280, ]; name_pet_h <- unique(pet_h$country)  ## high

pet_H <- subset(events_sum, country %in% name_pet_h)
pet_high <- fixest::feglm(data=pet_H, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(pet_high)
pet_L <- subset(events_sum, country %in% name_pet_l)
pet_low <- fixest::feglm(data=pet_L, count~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(pet_low)


## TYPE OF CONFLICT
state <- subset(ge, type=="state")
model <- fixest::feglm(data=state, conflicts~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(model)
Nstate <- subset(ge, type=="Nstate")
model <- fixest::feglm(data=Nstate, conflicts~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(model)
onesided <- subset(ge, type=="onesided")
model <- fixest::feglm(data=onesided, conflicts~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella <- etable(model)


#################################################################################################
####  CONFLICTS ANALYSIS  ####
#################################################################################################

gm <- read.csv("^Data/gws_migr.csv")
data_1 <- subset(gm, interval==1); data_5 <- subset(gm, interval==5)

## GLOBAL
model1 <- fixest::feglm(data=data_1, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
model5 <- fixest::feglm(data=data_5, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(model1); tabella5 <- etable(model5)


## GEOGRAPHIC SUBDIVISION
continent <- "Africa"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Africa1 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Africa1)
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Africa5 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Africa5) 
continent <- "Asia"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Asia1 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Asia1)
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Asia5 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
continent <- "Europe"
get_continent <- function(countries) {
  countrycode(countries, "country.name", "continent")}
data_continent <- data_1 %>%
  filter(get_continent(country) == continent)
Europe1 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Europe1)
data_continent <- data_5 %>%
  filter(get_continent(country) == continent)
Europe5 <- fixest::feglm(data=data_continent, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Europe5)
North_America <- c("United States", "Canada", "Mexico")
N_A <- data_1[data_1$country %in% North_America, ]
Namerica1 <- fixest::feglm(data=N_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Namerica1)
N_A <- data_5[data_5$country %in% North_America, ]
Namerica5 <- fixest::feglm(data=N_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Namerica5)
South_America <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela", "Aruba", "Falkland Islands", "French Guiana", "South Georgia and the South Sandwich Islands")
S_A <- data_1[data_1$country %in% South_America, ]
Samerica1 <- fixest::feglm(data=S_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Samerica1)
S_A <- data_5[data_5$country %in% South_America, ]
Samerica5 <- fixest::feglm(data=S_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Samerica5)
Central_America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Bahamas", "Barbados", "Cuba", "Dominica", "Jamaica", "Haiti", "Trinidad and Tobago", "Sint Maarten", "Saint Vincent and the Grenadines", "Saint Lucia", "Saint Kitts and Nevis", "Puerto Rico", "Dominican Republic", "Grenada", "Martinique", "Saint Martin", "Virgin Islands", "Turks and Caicos Islands", "Cayman Islands", "British Virgin Islands", "Guadeloupe", "Antigua and Barbuda", "Bonaire", "Curacao", "Saint Barthelemy", "Saba", "Saint Eustatius", "Saint Pierre and Miquelon", "British West Indies")
C_A <- data_1[data_1$country %in% Central_America, ]
Camerica1 <- fixest::feglm(data=C_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(Camerica1)
C_A <- data_5[data_5$country %in% Central_America, ]
Camerica5 <- fixest::feglm(data=C_A, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(Camerica5)

# SAME LIST OF COUNTIRES FOR: MENA, SUB-SAHARAN AFRICA, SOUTH-EAST ASIA, CENTRAL AND SOUTH AMERICA
data <- subset(data_1, interval==1)
mena <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella1 <- etable(mena)
data <- subset(data_1, interval==5)
mena <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella5 <- etable(mena)
data <- subset(data_2, interval==1)
sub_sahara <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella1 <- etable(sub_sahara)
data <- subset(data_2, interval==5)
sub_sahara <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella5 <- etable(sub_sahara)
data <- subset(data_3, interval==1)
sud_est_asia <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella1 <- etable(sud_est_asia) 
data <- subset(data_3, interval==5)
sud_est_asia <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella5 <- etable(sud_est_asia)
data <- subset(data_4, interval==1)
cs_america <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella1 <- etable(cs_america)
data <- subset(data_4, interval==5)
cs_america <- fixest::feglm(data = data, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=quasipoisson)
tabella5 <- etable(cs_america)


## INCOME

gm_high <- subset(gm_1, country %in% name_high)
high1 <- fixest::feglm(data=gm_high, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(high1)
gm_high <- subset(gm_5, country %in% name_high)
high5 <- fixest::feglm(data=gm_high, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(high5)
gm_low <- subset(gm_1, country %in% name_low)
low1 <- fixest::feglm(data=gm_low, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(low1)
gm_low <- subset(gm_5, country %in% name_low)
low5 <- fixest::feglm(data=gm_low, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(low5)
gm_highmid <- subset(gm_1, country %in% name_highmid)
midhigh1 <- fixest::feglm(data=gm_highmid, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(midhigh1)
gm_highmid <- subset(gm_5, country %in% name_highmid)
midhigh5 <- fixest::feglm(data=gm_highmid, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(midhigh5)
gm_lowmid <- subset(gm_1, country %in% name_lowmid)
lowmid1 <- fixest::feglm(data=gm_lowmid, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(lowmid1)
gm_lowmid <- subset(gm_5, country %in% name_lowmid)
lowmid5 <- fixest::feglm(data=gm_lowmid, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(lowmid5)


## GOVERNANCE
gm_gov1 <- subset(gm_1, country %in% name_gov1)
gov11 <- fixest::feglm(data=gm_gov1, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(gov11)
gm_gov1 <- subset(gm_5, country %in% name_gov1)
gov15 <- fixest::feglm(data=gm_gov1, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(gov15)
gm_gov2 <- subset(gm_1, country %in% name_gov2)
gov21 <- fixest::feglm(data=gm_gov2, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(gov21)
gm_gov2 <- subset(gm_5, country %in% name_gov2)
gov25 <- fixest::feglm(data=gm_gov2, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(gov25)
gm_gov3 <- subset(gm_1, country %in% name_gov3)
gov31 <- fixest::feglm(data=gm_gov3, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(gov31)
gm_gov3 <- subset(gm_5, country %in% name_gov3)
gov35 <- fixest::feglm(data=gm_gov3, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(gov35)


## PET
pet_H <- subset(gm_1, country %in% name_pet_h)
pet_high1  <- fixest::feglm(data=pet_H, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(pet_high1)
pet_H <- subset(gm_5, country %in% name_pet_h)
pet_high5  <- fixest::feglm(data=pet_H, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(pet_high5)
pet_L <- subset(gm_1, country %in% name_pet_l)
pet_low1 <- fixest::feglm(data=pet_L, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella1 <- etable(pet_low1)
pet_L <- subset(gm_5, country %in% name_pet_l)
pet_low5 <- fixest::feglm(data=pet_L, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella5 <- etable(pet_low5)












