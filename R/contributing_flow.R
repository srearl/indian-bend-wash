
# README ------------------------------------------------------------------

# The workflow detailed here takes MCFCD discharge data scraped (note,
# hand-scraped as MCFCD does not have an API and the web data harvest is via a
# non-paramaterized PHP script) for, in this case, MCFCD gauges that are within
# the Indian Bend Wash catchment, brackets individual storms, assigns a unique
# storm id to each storm, calculates cumulative discharge (L), and
# integrates with IBW.

# As of 2018-02-03, 09512162 data are provisional through 2017-07-05 08:45


# libraries ---------------------------------------------------------------
library(tidyverse)
library(sqldf)


# functions ---------------------------------------------------------------
source()


# data ingestion ----------------------------------------------------------

# identify data entities
f4603 <- 'https://www.dropbox.com/s/dwa03pc9uaoe06c/4603?dl=1'
f4613 <- 'https://www.dropbox.com/s/wli7wrln3mgpd4y/4613?dl=1'
f4618 <- 'https://www.dropbox.com/s/wxx26gq04stx1on/4618?dl=1'
f4623 <- 'https://www.dropbox.com/s/tu5nalfh6k58we3/4623?dl=1'
f4628 <- 'https://www.dropbox.com/s/v06vpkginiplswh/4628?dl=1'
f4643 <- 'https://www.dropbox.com/s/9uxepon9yhma145/4643?dl=1'
f4678 <- 'https://www.dropbox.com/s/tvt9aydsvuvl4m5/4678?dl=1'
f4688 <- 'https://www.dropbox.com/s/7l1wldk5r6oaleo/4688?dl=1'
f4693 <- 'https://www.dropbox.com/s/6zt89fjw9490lve/4693?dl=1'
f4728 <- 'https://www.dropbox.com/s/oawg3kv9eknulbg/4728?dl=1'

# list data entities

# scrapedData <- c(f4603, f4613) # small, development subset
scrapedData <- c(f4603, f4613, f4618, f4623, f4628, f4643, f4678, f4688, f4693, f4728)


# MCFCD data import -------------------------------------------------------

# function to import data entity, and format as required, currently hard-coded
# to scraped MCFCD file format - see README
data_import <- function(mcfcd_data_file) {
  
  read_delim(mcfcd_data_file, 
             delim = "not_set",
             skip = 8) %>% 
    separate(col=1, 
             into=c("dateTime", "feet", "cfs"),
             sep="\\s{2,}") %>% 
    filter(!is.na(cfs)) %>% 
    mutate(
      site = basename(mcfcd_data_file),
      dateTime = as.POSIXct(dateTime, format="%m/%d/%Y %H:%M:%S") 
    ) %>% 
    select(site, dateTime, cfs) %>% 
    arrange(dateTime)
  
}

# import all of the scraped data
importedData <- lapply(scrapedData, data_import)


# storm delineation -------------------------------------------------------

# storm_delineation: function to bracket storms, and give each storm a unique
# identifier. Input is a tabular hydrograph. Function is currently hard-coded to
# use 'cfs' as the discharge term, and the function would be much more robust if
# this term was paramaterized.
storm_delineation <- function(imported_data) {
  
  # delinate the record to distinct storms
  j <- 1
  k <- 2
  while (j != k) {
    j <- nrow(imported_data)
    i <- 1
    for (i in 1:nrow(imported_data)-1) {
      ifelse(imported_data[i-1,]$cfs == 0 && imported_data[i,]$cfs == 0 && imported_data[i+1,]$cfs == 0, imported_data <- imported_data[-i,], imported_data <- imported_data)
    }
    k <- nrow(imported_data)
  }
  
  # omit the first and last rows, which will not be caught by the above loops
  imported_data <- imported_data[-1,]
  highestRecord <- nrow(imported_data)
  imported_data <- imported_data[-highestRecord,]
  
  # add a field for marking the start of storms
  imported_data <- imported_data %>%
    mutate(stormMark = NA)
  
  # mark the start of all new storms with a 1
  for (i in 2:nrow(imported_data)) {
    if(imported_data[i-1,]$cfs == 0 & imported_data[i,]$cfs == 0) {
      imported_data[i,]$stormMark = 1
    }
  }
  
  # create unique storm marks of sequential values
  k <- 0
  for (i in 1:nrow(imported_data)) {
    if(!is.na(imported_data[i,]$stormMark)) {
      imported_data[i,]$stormMark = imported_data[i,]$stormMark + k
      k = k + 1
    }
  }
  
  # populate all rows with corresponding stormMark
  imported_data[1,]$stormMark <- 0 # initialize first storm with id = 0
  
  imported_data <- imported_data   %>%
    mutate(stormMark = zoo::na.locf(stormMark))
  
} # close storm_delination

# delinate storms from all MCFCD sites
delineatedData <- lapply(importedData, storm_delineation)

# remove single-line storm from site 4688
delineatedData[[8]] <- delineatedData[[8]] %>%
  filter(stormMark != 209)

# adjust start and end times of storms ------------------------------------

# adjust_duration: function to adjust the start and end of each storm (as noted
# by the zeroes bracketing first and last measured flow) to within 15 minutes of
# first and last flow. This adjustment is required as the time lapse between the
# first (or last) observed value and the preceding (or following) zero flow can
# be very long - days, even weeks. To better estimate the cumulative volume of
# water between zero and the first (or last) observed flow, the time of the zero
# points bracketing observed flow is forced to within 15 minutes (900 seconds)
# of observed flow.

adjust_duration <- function(delineated_data) {
  
  delineated_data <- delineated_data %>% 
    mutate(
      stormMark = as.integer(stormMark)
    ) %>%
    group_by(stormMark) %>% 
    mutate(
      deltaTime = difftime(dateTime, lag(dateTime), units = "secs")
    ) %>% 
    ungroup()
  
  delineated_data$dateTimeMod <- delineated_data$dateTime
  
  for (i in unique(delineated_data$stormMark)) {
    
    # if time between start of storm and preceding 0 is > 15 minutes, set time
    # of preceding 0 to 15 minutes before first discharge reading
    if (delineated_data[delineated_data$stormMark == i,]$deltaTime[[2]] > 900) {
      delineated_data[delineated_data$stormMark == i,]$dateTimeMod[[1]] <- delineated_data[delineated_data$stormMark == i,]$dateTime[[2]] - minutes(15)
    }
    
    # if time between end of storm and trailing 0 is > 15 minutes, set time of
    # trailing 0 to 15 minutes after last discharge reading
    stormTimeSteps <- nrow(delineated_data[delineated_data$stormMark == i,]) 
    
    if (delineated_data[delineated_data$stormMark == i,]$deltaTime[[stormTimeSteps]] > 900) {
      delineated_data[delineated_data$stormMark == i,]$dateTimeMod[[stormTimeSteps]] <- delineated_data[delineated_data$stormMark == i,]$dateTime[[stormTimeSteps - 1]] + minutes(15)
    }
    
  } # close loop through stormMarks
  
  # compute new time differences on adjusted date time
  delineated_data <- delineated_data %>% 
    group_by(stormMark) %>% 
    mutate(
      deltaTimeMod = difftime(dateTimeMod, lag(dateTimeMod), units = "secs")
    ) %>% 
    ungroup()
  
  return(delineated_data)
  
} # close adjust_duration 

adjustedDuration <- lapply(delineatedData, adjust_duration)


# calculate cumulative discharge ------------------------------------------

# cumulative_discharge: function to calculate cumulative discharge for each
# storm.

cumulative_discharge <- function(delineated_data) {
  
  delineated_data %>% 
    mutate(
      Qls = as.numeric(cfs) * 28.316847, # cfs to L/sec
      vol = Qls * as.numeric(deltaTimeMod) # compute volume
    ) %>% 
    group_by(stormMark) %>% 
    mutate(
      cumQ = round(cumsum(ifelse(is.na(vol), 0, vol)), 0) # cumulative Q
    ) %>% 
    # distill storms to total volume
    summarise(
      site = max(site),
      stormStart = min(dateTimeMod),
      stormEnd = max(dateTimeMod),
      cumQ = max(cumQ)
    ) %>% 
    ungroup() %>%
    filter(cumQ != 0)
  
}

flowData <- lapply(adjustedDuration, cumulative_discharge)


# ibw hydro (from ibwQchem) -----------------------------------------------

# Need to relate IBW storm marks with subcatchment data. Here using hydro data
# in ibwQchem but any of the IBW data would work so long as the stormMarks are
# accuracte/consistent.

ibwQchem <- read_csv('https://www.dropbox.com/s/4xv8q1pt6jpvkje/ibwQchem.csv?dl=1') # ibwQchem.csv
ibwHydro <- ibwQchem %>% 
  select(stormMark, dateTime, cumQ) %>% 
  group_by(stormMark) %>% 
  summarise(
    ibwBegin = min(dateTime),
    ibwEnd = max(dateTime),
    cumQibw = max(cumQ)
  )


# marry MCFCD and IBW data ------------------------------------------------

# merge_ibw_data: function to mate subcatchment flow data with corresponding IBW
# flow. Merge is accomplished by marrying subcatchment flow data within the date
# range of IBW storms. Percent of flow at subcatchments relative to total flow
# at IBW for that storm (at IBW!) is calculated.

merge_ibw_data <- function(cum_Q_data) {
  
  targetFile <- cum_Q_data
  
  sqldf('
      SELECT
        tf.site,
        tf.stormStart,
        tf.stormEnd,
        ih.ibwBegin,
        ih.ibwEnd,
        tf.stormMark AS stormMark_sub,
        tf.cumQ AS cumQsub,
        ih.stormMark,
        ih.cumQibw
      FROM
        targetFile tf
      JOIN
        ibwHydro ih ON (tf.stormStart BETWEEN ih.ibwBegin and ih.ibwEnd OR tf.stormEnd BETWEEN ih.ibwBegin and ih.ibwEnd);
      ') %>% 
    group_by(stormMark) %>% 
    summarise(
      subcatchment = max(site),
      numSubCatchmentStorms = n(),
      subcatchmentStorms = list(stormMark_sub), # contributing storm marks
      cumQsubcatchment = sum(cumQsub),
      cumQibw = max(cumQibw)
    ) %>% 
    mutate(
      subcatchmentStorms = paste(subcatchmentStorms),
      percentOfFlow = round((cumQsubcatchment / cumQibw) * 100, digits = 1)
    )
  
} # close merge_ibw_data  

mergedSubCurryData <- lapply(flowData, merge_ibw_data)


# list to tibble ----------------------------------------------------------

# combine list of data frames

contributingFlow <- bind_rows(mergedSubCurryData)


# plotting ----------------------------------------------------------------

# plot data

contributingFlow %>% 
  filter(stormMark %in% c(9, 10, 11, 14, 15, 16, 17, 29, 32, 33, 34, 37, 39, 42, 22, 67, 74)) %>% 
  mutate(percentOfFlow = log1p(percentOfFlow)) %>% 
  ggplot(aes(x = stormMark, y = subcatchment)) +
  geom_raster(aes(fill = percentOfFlow))
