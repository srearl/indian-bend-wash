
# README ------------------------------------------------------------------

# Centralized location for importing data from files.


# libraries ---------------------------------------------------------------

library(tidyverse)


# options -----------------------------------------------------------------

options(scipen = 999)


# ibwQchem ----------------------------------------------------------------

# import data from Dropbox, and remove analytes with a very small number of
# samples

# note specifying the time zone on import, which is critical to maintaining the
# integrity of these data as readr will otherwise convert dates/times to UTC
# without warning nor shifting the corresponding data accordingly

ibwQchem <- read_csv('https://www.dropbox.com/s/wsseakmze4hsnws/ibwQchem.csv?dl=1',
                     locale = locale(tz = "America/Phoenix")) %>%  # note !!!
  mutate(concentration = as.numeric(concentration)) %>% 
  filter(!analyte %in% c('SO4D_IC', 'NiD_ICP', 'PbD_ICP', 'CaD_FLAME_AA', 'NO3D_IC'))


# set storms for analysis -------------------------------------------------

# at this point, we need to identify target storms for analysis, two approaches:

# 1. any storm with chem data (uncomment for this option)
# storms_with_chem <- ibwQchem %>% 
#   group_by(stormMark, analyte) %>%
#   filter(any(!is.na(concentration))) %>%
#   ungroup() %>%
#   distinct(stormMark) %>%
#   pull()

# 2. storms with reasonable sample-data coverage (default)
storms_with_chem <- c(9, 10, 11, 14, 15, 16, 17, 29, 32, 33, 34, 37, 39, 42, 44, 67, 74, 88, 91, 93, 94)

ibwQchem <- ibwQchem %>% 
  filter(stormMark %in% storms_with_chem)


# ibwQminute --------------------------------------------------------------

ibwQminute <- read_csv('https://www.dropbox.com/s/mhh6wd6fyq1ljxp/ibwQminute.csv?dl=1',
                       locale = locale(tz = "America/Phoenix")) # %>% 
  # filter(stormMark %in% storms_with_chem)


# contributing flow -------------------------------------------------------

contributingGauges <- read_csv('https://www.dropbox.com/s/7mgz8ajt0f8788i/contributing_gauges.csv?dl=1') %>% 
  select(-subcatchmentList, - subcatchmentStorms, -cumQibw) %>% 
  mutate(
    reachLength = as.factor(reachLength),
    fromGraniteReef = as.factor(fromGraniteReef),
    fromInterceptor = as.factor(fromInterceptor),
    fromBerneil = as.factor(fromBerneil),
    fromLakeMarguerite = as.factor(fromLakeMarguerite)
  )


# rainfall ----------------------------------------------------------------

# import Katie's preciptation estimates
katiePrecip <- read_csv('https://www.dropbox.com/s/yz1iwkdljecruqs/ibwRainfall.csv?dl=1') %>% 
  mutate(arcGisNN = rainfall_inches * 25.4) %>% 
  dplyr::select(-rainfall_inches)

# import R-derived IDW preciptation estimates
ibwPrecipitation <- read_csv('https://www.dropbox.com/s/zwlrzpffxey6a6e/ibwPrecipitation.csv?dl=1')