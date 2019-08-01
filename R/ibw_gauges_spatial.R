
# README ------------------------------------------------------------------

# The workflow here details using MCFCD station metadata to generate a spatial
# vector file of those locations. Station metadata are in a file titled
# ALERT_sensors_all_by_ID.csv that was downloaded from the MCFCD website. The
# bulk of the workflow here is to convert the lat long data provided by MCFCD as
# dms to dd.

# In addition to plotting, the spatial location data are used to measure the IBW
# reach length. The distances between stations along the thalweg of the wash
# from Curry (0 km) to Sweetwater. Those measurements are detailed in
# reach_length.csv.


# libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)


# generate kml of MCFCD stations ------------------------------------------

ibwGuages <- c(4603,
               4613,
               4618,
               4623,
               4628,
               4643,
               4678,
               4688,
               4693,
               4728)

# read MCFCD station metadata in file: `ALERT_sensors_all_by_ID.csv`
MCFCD_station_metadata <- read_csv('https://www.dropbox.com/s/j2ep0idsroir0tn/ALERT_sensors_all_by_ID.csv?dl=1')

MCFCD_station_metadata %>% 
  filter(DEV_ID %in% ibwGuages) %>% 
  separate(STA_LAT_DMS, c("lat_d", "lat_m", "lat_s"), sep = " ") %>% 
  separate(STA_LONG_DMS, c("long_d", "long_m", "long_s"), sep = " ") %>% 
  # manually add the USGS station location
  add_row(
    STA_NAME = "IBW @ Curry Rd.",
    lat_d = "33",
    lat_m = "26",
    lat_s = "25.75",
    long_d = "111",
    long_m = "54",
    long_s = "55.5",
  ) %>% 
  mutate(
    lat = as.numeric(lat_d) + (as.numeric(lat_m)/60) + (as.numeric(lat_s)/3600),
    lon = (as.numeric(long_d) + (as.numeric(long_m)/60) + (as.numeric(long_s)/3600))*-1
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_write("~/Desktop/ibwGauges.kml", driver='kml')
