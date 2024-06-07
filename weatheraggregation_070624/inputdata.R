################################################################################
#                   Author: Joshua Thompson
#   O__  ----       Email:  joshua.thompson@ofwat.gov.uk
#  c/ /'_ ---
# (*) \(*) --
# ======================== Script  Information =================================
# PURPOSE: weather and performance app 
#
# PROJECT INFORMATION:
#   Name: weather and performance app  
#
# HISTORY:----
#   Date		        Remarks
#	-----------	   ---------------------------------------------------------------
#	 23/05/2024    Created script                                   JThompson (JT)
#===============================  Environment Setup  ===========================
#==========================================================================================



# libraries for handling http requests and json files  
library(httr)
library(jsonlite)

# libraries for data manipulation and visualisations 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(extrafont)
library(stringr)
library(RColorBrewer)
library(plotly)
library(kableExtra)
library(DT)

# libraries for geospatial data 
library(sf)
library(sp)
library(arcpullr)

#############################
# Get list of stations 
#############################
# API URL
#url <- "https://environment.data.gov.uk/hydrology/id/open/stations"
url <-"https://environment.data.gov.uk/hydrology/id/open/stations?from=2009-04-01&to=2024-04-01&status.label=Active&observedProperty=rainfall&_limit=2000000"

# Make API request
response <- GET(url)
# Check if request was successful
if (http_status(response)$category == "Success") {
  # Parse JSON response
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  # Extract relevant data
  station_data <- data$items
  station_df <- tibble(stationName = station_data$label,
                       easting = station_data$easting,
                       northing = station_data$northing,
                       lat = station_data$lat,
                       long = station_data$long,
                       stationReference = station_data$stationReference,
                       wiskiID = station_data$wiskiID)
  
  # Convert to sf object
  coordinates <- station_df[, c("long", "lat")]
  colnames(coordinates) <- c("X", "Y")
  station_sf <- st_as_sf(station_df, coords = c("long", "lat"), crs = st_crs(4326))
  
  # Print result
  plot(station_sf$geometry)
} else {
  print("Error: API request failed")
}

#############################
# Get company boundaries 
#############################
# read in the uk water companies 
UKwatercompanies <- get_spatial_layer("https://services-eu1.arcgis.com/XxS6FebPX29TRGDJ/arcgis/rest/services/UC2/FeatureServer/0")

# so what is this data stored as?
class(UKwatercompanies) # an sf object from the simple features package

# we can use filter from the dplyr package to filter our geospatial data 
WCPR_Companies <- UKwatercompanies 

# we can check the projection of the feature class 
prj <- st_crs(WCPR_Companies)

# we can check the geometry to see if there's any slivers of geospatial impurities
validity <- st_is_valid(WCPR_Companies)

# oh dear, impure geometry geometry exists, but we can quickly repair that
WCPR_Companies <- st_make_valid(WCPR_Companies)

#############################
# spatial join to find out where gauges are
#############################

# spatial join
station_sf.company <- st_join(station_sf, WCPR_Companies,join = st_intersects, left = FALSE) %>% select(-c("WARNINGS", "Shape__Area","Shape__Length"))


# convert to a flat table
company_gauges_ft <- joined_data  %>%
  ungroup() %>% as_tibble() %>% select(-c("WARNINGS", "Shape__Area","Shape__Length"))

#save data 

saveRDS(WCPR_Companies, "C:/Users/Joshua.Thompson/OneDrive - OFWAT/Documents/R Scripts/Weather App/WCPR_Companies.rds")
saveRDS(station_sf.company, "C:/Users/Joshua.Thompson/OneDrive - OFWAT/Documents/R Scripts/Weather App/station_sf.rds")

area_cents <- st_centroid(WCPR_Companies) %>% select(c(AreaServed,Acronym)) %>% bind_cols(as.data.frame(st_coordinates(st_centroid(WCPR_Companies))) %>% rename(long = 1,  lat = 2)) %>% select(-c(geoms))
saveRDS(area_cents, "C:/Users/Joshua.Thompson/OneDrive - OFWAT/Documents/R Scripts/Weather App/area_cents.rds")



##################################################
##################################################


library(sf)
library(dplyr)

# Create a data frame with the station information
stations <- data.frame(
  Name = c("Aberporth", "Armagh", "Ballypatrick Forest", "Bradford", "Braemar No 2", "Camborne", 
           "Cambridge Niab", "Cardiff Bute Park", "Chivenor", "Cwmystwyth", "Dunstaffnage", 
           "Durham", "Eastbourne", "Eskdalemuir", "Heathrow", "Hurn", "Lerwick", "Leuchars", 
           "Lowestoft Monckton Avenue", "Manston", "Nairn Druim", "Newton Rigg", "Oxford", 
           "Paisley", "Ringway", "Ross-on-wye", "Shawbury", "Sheffield", "Southampton Mayflower Park", 
           "Stornoway Airport", "Sutton Bonington", "Tiree", "Valley", "Waddington", "Whitby", 
           "Wick Airport", "Yeovilton"),
  Longitude = c(-4.57, -6.649, -6.153, -1.772, -3.396, -5.327, 0.102, -3.187, -4.147, -3.802, -5.439, 
                -1.585, 0.285, -3.205, -0.449, -1.835, -1.183, -2.861, 1.727, 1.337, -3.821, -2.786, 
                -1.262, -4.43, -2.279, -2.584, -2.663, -1.49, -1.408, -6.318, -1.25, -6.88, -4.535, 
                -0.522, -0.624, -3.088, -2.641),
  Latitude = c(52.139, 54.352, 55.181, 53.813, 57.011, 50.218, 52.245, 51.488, 51.089, 52.358, 56.451, 
               54.768, 50.759, 55.312, 51.479, 50.779, 60.139, 56.377, 52.483, 51.346, 57.593, 54.67, 
               51.761, 55.846, 53.356, 51.911, 52.794, 53.381, 50.898, 58.214, 52.836, 56.5, 53.252, 
               53.175, 54.481, 58.454, 51.006),
  Start_Year = c(1941, 1853, 1961, 1908, 1959, 1978, 1959, 1977, 1951, 1959, 1971, 1880, 1959, 1911, 
                 1948, 1957, 1931, 1957, 1914, 1934, 1931, 1959, 1853, 1959, 1946, 1931, 1946, 1883, 
                 1855, 1873, 1959, 1928, 1931, 1947, 1961, 1914, 1964),
  link = c("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/aberporthdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/armaghdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/ballypatrickdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/bradforddata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/braemardata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/cambornedata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/cambridgedata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/cardiffdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/chivenordata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/cwmystwythdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/dunstaffnagedata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/durhamdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/eastbournedata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/eskdalemuirdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/heathrowdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/hurndata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/lerwickdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/leucharsdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/lowestoftdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/manstondata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/nairndata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/newtonriggdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/oxforddata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/paisleydata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/ringwaydata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/rossonwyedata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/shawburydata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/sheffielddata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/southamptondata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/stornowaydata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/suttonboningtondata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/tireedata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/valleydata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/waddingtondata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/whitbydata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/wickairportdata.txt",
           "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/yeoviltondata.txt")
)

# Convert the data frame to an sf object
temp_stations_sf <- st_as_sf(stations %>% mutate(lat =Latitude, lng=Longitude), coords = c("Longitude", "Latitude"), crs = 4326)

# Display the sf object
print(temp_stations_sf)


#############################
# spatial join to find out where gauges are
#############################

# Find the nearest temperature station for each company
nearest_indices <- st_nearest_feature(WCPR_Companies, temp_stations_sf)

# Add the index of the nearest temperature station to WCPR_Companies
WCPR_Companies <- WCPR_Companies %>%
  mutate(nearest_temp_station = nearest_indices)

# Join the company data with the nearest temperature station data
rain_station_sf_company <- WCPR_Companies %>%
  left_join(temp_stations_sf %>% mutate(row_id = row_number()), 
            by = c("nearest_temp_station" = "row_id"))

# Select relevant columns and remove unnecessary ones
rain_station_sf_company <- rain_station_sf_company %>%
  select(-c("WARNINGS", "Shape__Area", "Shape__Length"))

#save data 

saveRDS(WCPR_Companies, "C:/Users/Joshua.Thompson/OneDrive - OFWAT/Documents/R Scripts/Weather App/WCPR_Companies_temp.rds")
saveRDS(temp_stations_sf, "C:/Users/Joshua.Thompson/OneDrive - OFWAT/Documents/R Scripts/Weather App/temp_station_sf.rds")




#############################
# Get list of stream stations 
#############################
# API URL
#url <- "https://environment.data.gov.uk/hydrology/id/open/stations"
url <-"https://environment.data.gov.uk/hydrology/id/open/stations?from=2009-04-01&to=2024-04-01&status.label=Active&observedProperty=waterFlow&_limit=2000000"

# Make API request
response <- GET(url)
# Check if request was successful
if (http_status(response)$category == "Success") {
  # Parse JSON response
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  
  
  # Extract relevant data
  station_data <- data$items
  station_data$label[[135]] <- "Beach's Mill"
  station_df <- tibble(stationName = unlist(station_data$label),
                       easting = station_data$easting,
                       northing = station_data$northing,
                       lat = station_data$lat,
                       long = station_data$long,
                       stationReference = station_data$stationReference,
                       wiskiID = station_data$wiskiID,
                       catchmentArea = station_data$catchmentArea)
  
  # Convert to sf object
  coordinates <- station_df[, c("long", "lat")]
  colnames(coordinates) <- c("X", "Y")
  station_sf <- st_as_sf(station_df, coords = c("long", "lat"), crs = st_crs(4326))
  
  # Print result
  plot(station_sf$geometry)
} else {
  print("Error: API request failed")
}

station_sf <- station_sf %>% filter(!is.na(catchmentArea)) 

#############################
# Get company boundaries 
#############################
# read in the uk water companies 
UKwatercompanies <- get_spatial_layer("https://services-eu1.arcgis.com/XxS6FebPX29TRGDJ/arcgis/rest/services/UC2/FeatureServer/0")

# so what is this data stored as?
class(UKwatercompanies) # an sf object from the simple features package

# we can use filter from the dplyr package to filter our geospatial data 
WCPR_Companies <- UKwatercompanies 

# we can check the projection of the feature class 
prj <- st_crs(WCPR_Companies)

# we can check the geometry to see if there's any slivers of geospatial impurities
validity <- st_is_valid(WCPR_Companies)

# oh dear, impure geometry geometry exists, but we can quickly repair that
WCPR_Companies <- st_make_valid(WCPR_Companies)

#############################
# spatial join to find out where gauges are
#############################

# spatial join
station_sf.company <- st_join(station_sf, WCPR_Companies,join = st_intersects, left = FALSE) %>% select(-c("WARNINGS", "Shape__Area","Shape__Length"))


# convert to a flat table
company_gauges_ft <- joined_data  %>%
  ungroup() %>% as_tibble() %>% select(-c("WARNINGS", "Shape__Area","Shape__Length"))

#save data 

saveRDS(WCPR_Companies, "C:/Users/Joshua.Thompson/OneDrive - OFWAT/Documents/R Scripts/Weather App/WCPR_Companies_stream.rds")
saveRDS(station_sf.company, "C:/Users/Joshua.Thompson/OneDrive - OFWAT/Documents/R Scripts/Weather App/station_sf_stream.rds")


station_sf_stream <- readRDS("C:/Users/Joshua.Thompson/OneDrive - OFWAT/Documents/R Scripts/Weather App/station_sf_stream.rds")

#area_cents <- st_centroid(WCPR_Companies) %>% select(c(AreaServed,Acronym)) %>% bind_cols(as.data.frame(st_coordinates(st_centroid(WCPR_Companies))) %>% rename(long = 1,  lat = 2)) %>% select(-c(geoms))
#saveRDS(area_cents, "C:/Users/Joshua.Thompson/OneDrive - OFWAT/Documents/R Scripts/Weather App/area_cents.rds")


#############################
# Overflow Data
#############################


