# Select the country
country <- "Mexico"
# Select the year
y <- "2005"
# Select the raster
rast <- "rs"



suppressPackageStartupMessages({
  library(sf)              ## useful for spatial manipulations
  library(sp)              ## useful for spatial manipulations
  library(plyr )
  library(raster)          ## useful for working with raster data
  library(ncdf4)           ## useful for working with raster data
  library(exactextractr)   ## useful for extracting data from raster files
  library(dplyr)           ## useful for merging data sets
  library(stringr)
  library(reshape2)        ## useful for manipulating data sets
  library(ggplot2)         ## useful for data visualization
  library(ggrepel)         ## useful for labeling point plots in ggplot2
  library(lubridate)
  library(zoo)   
  library(foreign)
})

shp <- sf::read_sf("Data/Shapefile/shapefile.shp")
shp <- sf::st_transform(shp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
state <- subset(shp, CNTRY_NAME == country)    ## plot(state[,"geometry"])
state <- rename(state, region = ADMIN_NAME)
r <- raster::brick(paste0("Data/GW/",rast,"y.nc")); proj4string(r) <- raster::crs(shp)
path <- paste0("Data/GW_Conflict/", country, "/")
data_gw_events <- read.csv(paste0(path, country,"_gw_events_", rast,".csv"))
data_gw <- read.csv(paste0(path, country,"_gw_", rast,".csv"))
events <-read.csv(paste0(path, country, "_events.csv"))
print(unique(data_gw_events$region))

######################################################################################################
# Plot mappa GW anno scelto

data_year <- subset(data_gw, year == y)
data_year <- left_join(state, data_year, by = "region")

ggplot(data_year, aes(fill=value)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="GW storage [kg/m^2]") +
  scale_fill_viridis_c(option="viridis", end=0.8)+
  labs(title = paste(country, y))

######################################################################################################
# Plot serie temporali GW per regioni scelte

# Selezionare le regioni per le serie temporali
# R <- c("", "", "", "")
R <- unique(data_gw_events$region)  ## Se voglio vederle tutte insieme

ggplot(subset(data_gw_events, region %in% R), 
       aes(year, value)) +   
  facet_wrap(region~., ncol=4) +        
  theme_bw()+
  labs(title = paste(country),x="",y="GW Storage [kg/m^2]") +
  theme(strip.background=element_rect(fill="white")) +          
  geom_line()

######################################################################################################

# Plot data points over geometry figure
ggplot() +
  geom_sf(data = state)+
  geom_point(data = events, aes(x = longitude, y = latitude, color = type, size = number)) + 
  scale_color_discrete(name = "Type") +
  scale_color_manual(values = c("state" = "blue", "Nstate" = "red", "onesided" = "green")) +
  labs(title = "Events",x="",y="") +
  theme_bw() +                     
  theme(strip.background = element_rect(fill="white"))


######################################################################################################
# Plot della timeseries in una regione dei conflitti Nstate

reg <- "Nasarawa"

data <- subset(data_gw_events,region==reg)
data <- subset(data, type=="Nstate")
agg_data <- data %>%
  group_by(year, region) %>%
  summarise(count = sum(number))

ggplot(data = agg_data, aes(x = year, y = count)) +
  geom_line() +   # Aggiunge la linea
  theme_bw()+
  labs(title = "Events",x="",y="Number")

######################################################################################################
# Plot delle timeseries dei conflitti Nstate in varie regioni scelte 

  # reg <- unique(data_gw_events$region)  ## Se voglio vederle tutte insieme
reg <- c("Abia", "Adamawa", "Anambra", "Borno", "Edo", "Jigawa", "Nasarawa", "Ogun")
data <- data_gw_events
agg_data <- data %>%
  filter(type == "Nstate", region %in% reg) %>%
  group_by(year, region) %>%
  summarise(count = sum(number))

ggplot(data = agg_data, aes(x = year, y = count)) +
  geom_bar(stat = "identity") + 
  labs(x = "Year", y = "Count", title="Non-State Events") +
  theme_bw()+
  facet_wrap(~ region, ncol = 6)

######################################################################################################
# Plot della timeseries dei conflitti in una regione state+Nstate+onesided

reg <- "Chihuahua"
data <- subset(data_gw_events,region==reg)
agg_data <- data %>%
  group_by(year) %>%
  summarise(count = sum(number))

ggplot(data = agg_data, aes(x = year, y = count)) +
  geom_bar(stat = "identity") +
  theme_bw()+
  labs(title = paste(reg, "All Events"))+
  labs(x = "Year", y = "Count")

######################################################################################################
# plot timeseries dei conflitti in regioni scelte state+Nstate+onesided

# reg <- unique(data_gw_events$region)  ## Se voglio vederle tutte insieme
reg <- c("Abia", "Adamawa", "Anambra", "Borno", "Edo", "Jigawa", "Nasarawa", "Ogun")
data <- data_gw_events
agg_data <- data %>%
  filter(region %in% reg) %>%
  group_by(year, region) %>%
  summarise(count = sum(number))


ggplot(data = agg_data, aes(x = year, y = count)) +
  geom_line() +   # Aggiunge la linea
  labs(x = "Year", y = "Count") +
  facet_wrap(~ region, ncol = 3)

######################################################################################################
# PLOT GW+CONFLICTS IN ONE REGION state+Nstate+onesided

reg <- "Chihuahua"
data <- subset(data_gw_events,region==reg); data$geometry=NULL; data$CNTRY_NAME=NULL
agg_data <- data %>%
  group_by(year, region, value) %>%
  summarise(count = sum(number))
mvalue <- mean(agg_data$value)
svalue <- sd(agg_data$value)
mcount <- mean(agg_data$count)
scount <- sd(agg_data$count)
agg_data$Svalue <- (agg_data$value-mvalue)/svalue
agg_data$Scount <- (agg_data$count-mcount)/scount

ggplot(agg_data, aes(year)) +
  geom_line(aes(y = Svalue), colour = "blue", size=1) +
  geom_bar(aes(y = Scount), stat = "identity", fill = "red", alpha = 0.5) +
  scale_fill_manual(values = "red", name = "Scount") + 
  labs(title="Mexico GW Storage vs. All Conflicts (Normalized)", x = "Year", y = "", color = "Legend") + 
  theme_bw()

######################################################################################################
# PLOT GW+CONFLICTS SLECTED REGIONS state+Nstate+onesided

reg <- c("Abia", "Adamawa", "Anambra", "Borno", "Edo", "Jigawa", "Nasarawa", "Ogun")
# reg <- unique(data_gw_events$region)
data <- data_gw_events
agg_data <- subset(data, region==reg)
agg_data <- data %>%
  filter(region %in% reg) %>%
  group_by(year, region, value) %>%
  summarise(count = sum(number))

b<-data.frame()
for (i in reg){
  a <- subset(agg_data, region==i)
  mvalue <- mean(a$value)
  svalue <- sd(a$value)
  mcount <- mean(a$count)
  scount <- sd(a$count)
  a$Svalue <- (a$value-mvalue)/svalue
  a$Scount <- (a$count-mcount)/scount
  b <- rbind(b, a)
}
agg_data <- b

ggplot(data = agg_data, aes(year)) +
  geom_line(aes(y = Svalue), colour = "blue") +
  geom_line(aes(y = Scount), colour = "red")+
  facet_wrap(~ region, ncol = 3)







