library(ggplot2); library(sf)
# Select the country
country <- "Nigeria"
# Select the year
y <- "2000"
# Select the raster
r <- "rs"

path <- paste0("Data/GW_Conflict/", country, "/")
data_gw_events <- read.dbf(paste0(path, country,"_gw_events_", r,".dbf"))
events <-read.csv(paste0(path, country, "_events.csv"))
state <- subset(shp, CNTRY_NAME == country)    ## plot(state[,"geometry"])
state <- rename(state, region = ADMIN_NAME)

######################################################################################################
# Plot mappa GW anno scelto

data_year <- subset(data_gw_events, year == y)
data_year <- left_join(state, data_year, by = "region")

ggplot(data_year, aes(fill=value)) + 
  geom_sf(col="black") +
  theme_bw() +
  labs(fill="gw storage") +
  scale_fill_viridis_c(option="viridis", end=0.8)

######################################################################################################
# Plot serie temporali GW per regioni scelte

# Selezionare le regioni per le serie temporali
# R <- c("", "", "", "")
R <- unique(data_gw_events$region)  ## Se voglio vederle tutte insieme

ggplot(subset(data_gw_events, region %in% R), 
       aes(year, value, fill=value, col=value)) +   
  facet_wrap(region~., ncol=6) +        
  theme_bw() +         
  theme(strip.background=element_rect(fill="white")) +          
  ylab("") +                                                                
  xlab("") +                                                                       
  geom_col() +                                                                      
  scale_fill_viridis_c(option="viridis", end  = 0.8) +                              
  scale_color_viridis_c(option="viridis", end = 0.8)

######################################################################################################
# Plot data points over geometry figure

shape <- data_gw_events; shape$CNTRY_NAME=NULL; shape$year=NULL; shape$value=NULL;shape$type=NULL
shape <- shape %>%
  group_by(ADMIN_NAME, geometry) %>%
  summarise(count = sum(number)); shape$count=NULL
ggplot(shape) +           
  geom_sf(fill = NA, col = "black") +  
  geom_point(data = events, aes(longitude, latitude, color=type), size = .7) +
  # Assegna manualmente i colori ai valori di code_status
  scale_color_manual(values = c("state" = "red", "Nstate" = "blue", "onesided" = "green"))+
  theme_bw() +                     
  # white background for variable names
  theme(strip.background = element_rect(fill="white"))

######################################################################################################
# Plot della timeseries in una regione dei conflitti Nstate

print(unique(data_gw_events$ADMIN_NAME))
reg <- "Borno"

data <- subset(data_gw_events,ADMIN_NAME==reg); data$geometry=NULL
data <- subset(data, type=="Nstate")
agg_data <- data %>%
  group_by(year, ADMIN_NAME) %>%
  summarise(count = sum(number))

ggplot(data = agg_data, aes(x = year, y = count)) +
  geom_line() +   # Aggiunge la linea
  labs(x = "Year", y = "Count")

######################################################################################################
# Plot delle timeseries dei conflitti Nstate in varie regioni scelte 

# reg <- unique(data_gw_events$ADMIN_NAME)  ## Se voglio vederle tutte insieme
reg <- c("Abia", "Adamawa", "Anambra", "Borno", "Edo", "Jigawa", "Nasarawa", "Ogun")
data <- data_gw_events; data$geometry=NULL
agg_data <- data %>%
  filter(type == "Nstate", ADMIN_NAME %in% reg) %>%
  group_by(year, ADMIN_NAME) %>%
  summarise(count = sum(number))

ggplot(data = agg_data, aes(x = year, y = count)) +
  geom_line() +   # Aggiunge la linea
  labs(x = "Year", y = "Count") +
  facet_wrap(~ ADMIN_NAME, ncol = 3)

######################################################################################################
# Plot della timeseries dei conflitti in una regione state+Nstate+onesided

reg <- "Borno"
data <- subset(data_gw_events,ADMIN_NAME==reg); data$geometry=NULL
agg_data <- data %>%
  group_by(year) %>%
  summarise(count = sum(number))

ggplot(data = agg_data, aes(x = year, y = count)) +
  geom_line() +   # Aggiunge la linea
  labs(x = "Year", y = "Count")

######################################################################################################
# plot timeseries dei conflitti in regioni scelte state+Nstate+onesided

# reg <- unique(data_gw_events$ADMIN_NAME)  ## Se voglio vederle tutte insieme
reg <- c("Abia", "Adamawa", "Anambra", "Borno", "Edo", "Jigawa", "Nasarawa", "Ogun")
data <- data_gw_events; data$geometry=NULL
agg_data <- data %>%
  filter(ADMIN_NAME %in% reg) %>%
  group_by(year, ADMIN_NAME) %>%
  summarise(count = sum(number))


ggplot(data = agg_data, aes(x = year, y = count)) +
  geom_line() +   # Aggiunge la linea
  labs(x = "Year", y = "Count") +
  facet_wrap(~ ADMIN_NAME, ncol = 3)

######################################################################################################
# PLOT GW+CONFLICTS IN ONE REGION state+Nstate+onesided

reg <- "Borno"
data <- subset(data_gw_events,ADMIN_NAME==reg); data$geometry=NULL; data$CNTRY_NAME=NULL
agg_data <- data %>%
  group_by(year, ADMIN_NAME, value) %>%
  summarise(count = sum(number))
mvalue <- mean(agg_data$value)
svalue <- sd(agg_data$value)
mcount <- mean(agg_data$count)
scount <- sd(agg_data$count)
agg_data$Svalue <- (agg_data$value-mvalue)/svalue
agg_data$Scount <- (agg_data$count-mcount)/scount

ggplot(agg_data, aes(year)) +
  geom_smooth(aes(y = Svalue), colour = "blue") +
  geom_smooth(aes(y = Scount), colour = "red")

ggplot(agg_data, aes(year)) +
  geom_line(aes(y = Svalue), colour = "blue") +
  geom_line(aes(y = Scount), colour = "red")

######################################################################################################
# PLOT GW+CONFLICTS SLECTED REGIONS state+Nstate+onesided

reg <- c("Abia", "Adamawa", "Anambra", "Borno", "Edo", "Jigawa", "Nasarawa", "Ogun")
reg <- unique(data_gw_events$ADMIN_NAME)
data <- data_gw_events; data$geometry=NULL
agg_data <- data %>%
  filter(ADMIN_NAME %in% reg) %>%
  group_by(year, ADMIN_NAME, value) %>%
  summarise(count = sum(number))

b<-data.frame()
for (i in reg){
  a <- subset(agg_data, ADMIN_NAME==i)
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
  facet_wrap(~ ADMIN_NAME, ncol = 3)













