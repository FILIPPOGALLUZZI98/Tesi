# This is the code to prepare the datasets for global data from the modified shapefile and
# raster (see 100.Prep_Shapefile_Raster.R). This code will create the datasets of global variables.
# This is a list of the saved datasets:
# GW --> (year, country, region, value)
# Conflict --> (year, country, region, type, conflicts)  this contains only the observed ones, there are no zeros
# Deaths --> (year, country, region, type, number_deaths)  this contains only the observed ones, there are no zeros
# GW-Conflict --> (year, country, region, type, conflicts, value) the dataset contains also zeros in the number columns if in
                                                               # that year and region no conflict happened
# GW-Deaths --> (year, country, region, type, number_deaths, value) the dataset contains also zeros in the number columns if in
                                                               # that year and region no conflict happened
# Migr --> () 
# GW-Migr -->()


# Select the raster (gws, ...)
rast <- "gws"

##############################################################################################################################
##############################################################################################################################

suppressPackageStartupMessages({
  library(sf);library(sp);library(plyr);library(raster);library(ncdf4);library(exactextractr);library(dplyr);library(stringr)
  library(reshape2);library(ggplot2);library(ggrepel);library(lubridate);library(zoo);library(foreign)})

# Select shapefile and raster
shp <- st_read("^Data/shp/shp.shp")
r <- raster::brick(paste0("^Data/",rast,".nc"))


##############################################################################################
####  GLOBAL GW  #############################################################################

# Merging data gw-shapefile
gw_g <- exactextractr::exact_extract(r, shp, fun="mean")

# Add columns for regions and countries
gw_g$region <- shp$region ; gw_g$country <- shp$country

# Reshape the dataset into a long form
gw_g <- reshape2::melt(gw_g, id.vars=c("country", "region"))

# Rename the years
gw_g$variable <- gsub("mean.X", "", gw_g$variable)  # Rimuovi "mean.X"
gw_g$year <- as.integer(gsub("\\D", "", gw_g$variable)) + 1900 
gw_g$variable=NULL
gw_g <- gw_g[, c("year","country", "region", "value")]

# Save data
write.csv(gw_g, paste0("^Data/", "Global_",rast, ".csv"), row.names=FALSE)

##############################################################################################
####  GLOBAL CONFLICT UPPSALA (N_DEATHS+CONFLICTS)  ##########################################

# Select the conflict data
file_path <- "^Data_Raw/Conflict_Data/Global.csv"
events <- read.csv(file_path)

# Select the variables of interest
events <- events[, c("country" ,"year", "type_of_violence","latitude" ,"longitude", "best")]

# Rename the variables
events <- events %>%
  rename(type = type_of_violence,
         number_deaths = best)
events <- mutate(events,
                 type = case_when(
                   type == 1 ~ "state",
                   type == 2 ~ "Nstate",
                   type == 3 ~ "onesided"
                 ))

# Set the coordinate system
events <- st_as_sf(events, coords = c("longitude", "latitude"), crs = st_crs(shp))
events <- st_transform(events, st_crs(shp))

# Remove the invalid geometries from shp
shp <- shp[st_is_valid(shp), ]

# Intersection shapefile-events and aggregate data 
events_joined <- st_join(events, shp)
events_joined$country.x=NULL
events_joined <- events_joined %>%
  rename(country = country.y)
events_joined$geometry=NULL



events_deaths <- events_joined %>%
  group_by(year, country, region, type) %>%
  summarise(deaths = sum(number_deaths, na.rm = TRUE))
# Sort datasets by year
events_deaths <- events_deaths[order(events_deaths$country),]
events_deaths <- events_deaths[order(events_deaths$year),]

events_conflicts <- events_joined %>%
  group_by(year, country, region, type) %>%
  summarise(conflicts = n())
# Sort datasets by year
events_conflicts <- events_conflicts[order(events_conflicts$country),]
events_conflicts <- events_conflicts[order(events_conflicts$year),]

# Save data
write.csv(events_conflicts, paste0("^Data/", "Global_conflicts", ".csv"), row.names=FALSE)
write.csv(events_deaths, paste0("^Data/", "Global_deaths", ".csv"), row.names=FALSE)


#############################################################################################################################
####  GLOBAL MIGRATION DATASET  #############################################################################################


##############################################################################################################################
####  GLOBAL JOINT DATASET GW-(DEATHS+CONFLITCT)  ############################################################################

gw_data_g <- gw_g %>%
  filter(year > 1988)
events_deaths <- events_deaths %>%
  filter(year<2020)
events_conflicts <- events_conflicts %>%
  filter(year<2020)
vettore <- expand.grid(year=1989:2019, type=c("state","Nstate","onesided"))
gw_events_g <- left_join(gw_data_g, vettore, by=c("year"))


gw_deaths <- left_join(gw_events_g,events_deaths,by=c("country","region","year","type"))
gw_conflicts <- left_join(gw_events_g,events_conflicts,by=c("country","region","year","type"))
gw_deaths$deaths[is.na(gw_deaths$deaths)] = 0  ## Assign a zero to each month/province where no data is observed
gw_conflicts$conflicts[is.na(gw_conflicts$conflicts)] = 0  ## Assign a zero to each month/province where no data is observed
gw_deaths <- gw_deaths[, c("year","country", "region","type","deaths", "value")]
gw_conflicts <- gw_conflicts[, c("year","country", "region","type","conflicts", "value")]

# Save data
write.csv(gw_deaths, paste0("^Data/", "Global_deaths_gws", ".csv"), row.names=FALSE)
write.csv(gw_conflicts, paste0("^Data/", "Global_conflicts_gws", ".csv"), row.names=FALSE)









