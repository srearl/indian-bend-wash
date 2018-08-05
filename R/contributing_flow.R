
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
library(lubridate)


# options -----------------------------------------------------------------

options(scipen = 999)
options(sqldf.driver = "SQLite") # set sqldf to use SQLite driver


# data ingestion ----------------------------------------------------------

# identify data entities
f4603 <- 'https://www.dropbox.com/s/9sitzle3efck0z7/4603?dl=1'
f4613 <- 'https://www.dropbox.com/s/66y8dth8av7ingr/4613?dl=1'
f4618 <- 'https://www.dropbox.com/s/5r842sg6fax28bh/4618?dl=1'
f4623 <- 'https://www.dropbox.com/s/53epx5p6upvcqf5/4623?dl=1'
f4628 <- 'https://www.dropbox.com/s/ntuuzla6lag5rd4/4628?dl=1'
f4643 <- 'https://www.dropbox.com/s/q1yx8p2aytxb3xl/4643?dl=1'
f4678 <- 'https://www.dropbox.com/s/gu84ejjjdwyz2w4/4678?dl=1'
f4688 <- 'https://www.dropbox.com/s/razdcph6am0h7f2/4688?dl=1'
f4693 <- 'https://www.dropbox.com/s/1up8ko2s7f3akgb/4693?dl=1'
f4728 <- 'https://www.dropbox.com/s/3w4sa1qz1565sx9/4728?dl=1'

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

# Need to relate IBW storm marks with subcatchment data. Here using ibwQminute
# but any of the IBW data would work so long as the stormMarks are
# accuracte/consistent.

# use read.csv to avoid tidyverse conversion to UTC
ibwQminute <- read.csv('https://www.dropbox.com/s/mhh6wd6fyq1ljxp/ibwQminute.csv?dl=1', stringsAsFactors = FALSE) %>% 
  mutate(dateTime = as.POSIXct(dateTime, format = "%Y-%m-%d %H:%M:%S"))

ibwHydro <- ibwQminute %>% 
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

# update 2018-07-20: the SQL match used initially that identified MCFCD storms
# where the start or end of the storm were within the duration of an IBW storm
# (commented below) was found to be too restrictive. The SQL statement was
# updated to pair MCFCD and IBW storms if there was only overlap between the
# two.

#  on comparing date-range overlap:
#  https://stackoverflow.com/questions/325933/determine-whether-two-date-ranges-overlap

# original where-clause:
# ibwHydro ih ON (tf.stormStart BETWEEN ih.ibwBegin and ih.ibwEnd OR tf.stormEnd
# BETWEEN ih.ibwBegin and ih.ibwEnd);

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
        ibwHydro ih ON ((tf.stormStart <= ih.ibwEnd) AND (tf.stormEnd >= ih.ibwBegin));
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

contributingFlow <- bind_rows(mergedSubCurryData) %>% 
  arrange(stormMark, subcatchment)


# plotting ----------------------------------------------------------------

# percent of total for all storms

contributingFlow %>% 
  mutate(
    percentOfFlow = log1p(percentOfFlow),
    subcatchment = replace(subcatchment, grepl("4643", subcatchment), "sweetwater"),
    subcatchment = replace(subcatchment, grepl("4693", subcatchment), "shea"),
    subcatchment = replace(subcatchment, grepl("4688", subcatchment), "berneil"),
    subcatchment = replace(subcatchment, grepl("4678", subcatchment), "lakeMarguerite"),
    subcatchment = replace(subcatchment, grepl("4613", subcatchment), "silverado"),
    subcatchment = replace(subcatchment, grepl("4623", subcatchment), "interceptor"),
    subcatchment = replace(subcatchment, grepl("4628", subcatchment), "mcdonald"),
    subcatchment = replace(subcatchment, grepl("4618", subcatchment), "indianSchool"),
    subcatchment = replace(subcatchment, grepl("4728", subcatchment), "graniteReef"),
    subcatchment = replace(subcatchment, grepl("4603", subcatchment), "mckellips")
  ) %>% 
  # subcatchments to factor to control their position on the y-axis of the resulting plot
  mutate(subcatchment = factor(subcatchment, levels = c("mckellips",
                                                        "indianSchool",
                                                        "mcdonald",
                                                        "silverado",
                                                        "shea",
                                                        "sweetwater",
                                                        "graniteReef",
                                                        "interceptor",
                                                        "lakeMarguerite",
                                                        "berneil"))) %>% 
  ggplot(aes(x = stormMark, y = subcatchment, fill = percentOfFlow)) +
  geom_raster() +
  scale_fill_gradient(name="log(% flow @ Curry)") +
  ggtitle("flow within subcatchments as a % of total flow @ Curry\n* mckellips:sweetwater reflect reach length \n* graniteReef:berneil treated as contributing to the main channel",
          subtitle = "updated 2018-07-20 using a more inclusive match between MCFCD & IBW storms")
ggsave('~/Desktop/percent_of_flow_all_storms.png')


# plot discharge throughout catchment -------------------------------------

# function and workflow to generate plots of discharge at all sites throughout
# the IBW catchment based on IBW storm marks. Function requires output from
# above workflow, including: contributingStorms and adjustedDuration. In
# addition, interpolated IBW discharge is required, here: ibwQminute.

demarcated_subcatchments <- bind_rows(adjustedDuration) %>% 
  mutate(
    Qls = as.numeric(cfs) * 28.316847 # cfs to L/sec
  ) %>% 
  select(
    site, subStormMark = stormMark, dateTimeMod, Qls
  ) %>% 
  ungroup()

# use read.csv to avoid tidyverse conversion to UTC (if not already loaded from
# workflow above)

# ibwQminute <- read.csv('https://www.dropbox.com/s/sompxe82jrx1e4k/ibwQminute.csv?dl=1',
#                        stringsAsFactors = FALSE) %>%
#   mutate(dateTime = as.POSIXct(dateTime, format = "%Y-%m-%d %H:%M:%S"))

# joining on date so be sure time zones match - looking for ""
attr(ibwQminute$dateTime, "tzone")
attr(demarcated_subcatchments$dateTimeMod, "tzone")

# attr(ibwQminute$dateTime, "tzone") <- "America/Phoenix"
# attr(demarcated_subcatchments$dateTimeMod, "tzone") <- "America/Phoenix"

# plotting function
curry_subcatchments_discharge <- function(stormNumber) {
  
  contributingStorms <- tibble(
    site = as.character(NA),
    subStormMark = as.integer(NA),
    dateTimeMod = as.POSIXct(NA, format = "%Y-%m-%d %H:%M:%S"),
    Qls = as.numeric(NA)
  )
  
  for (i in 1:nrow(contributingFlow %>% filter(stormMark == stormNumber))) {
    
    siteid <- contributingFlow[contributingFlow$stormMark == stormNumber,]$subcatchment[[i]]
    subStorms <- contributingFlow[contributingFlow$stormMark == stormNumber,]$subcatchmentStorms[[i]]
    
    contributingStorms <- bind_rows(
      contributingStorms,
      demarcated_subcatchments[demarcated_subcatchments$site == siteid & demarcated_subcatchments$subStormMark == subStorms,]
    )
    
  }
  
  allSites <- bind_rows(
    contributingStorms %>%
      filter(!is.na(site)) %>% 
      mutate(
        stormMark = as.integer(stormNumber),
        site = replace(site, grepl("4643", site), "sweetwater"),
        site = replace(site, grepl("4693", site), "shea"),
        site = replace(site, grepl("4688", site), "berneil"),
        site = replace(site, grepl("4678", site), "lakeMarguerite"),
        site = replace(site, grepl("4613", site), "silverado"),
        site = replace(site, grepl("4623", site), "interceptor"),
        site = replace(site, grepl("4628", site), "mcdonald"),
        site = replace(site, grepl("4618", site), "indianSchool"),
        site = replace(site, grepl("4728", site), "graniteReef"),
        site = replace(site, grepl("4603", site), "mckellips")
      ) %>% 
      select(
        site,
        stormMark,
        subStormMark,
        dateTime = dateTimeMod,
        Qls
      ),
    ibwQminute %>% 
      filter(stormMark == stormNumber) %>% 
      mutate(
        site = 'ibw',
        stormMark = as.integer(stormMark),
        subStormMark = as.integer(NA)
      ) %>% 
      select(
        site,
        stormMark,
        subStormMark,
        dateTime,
        Qls
      )
  )
  
  allSites %>% 
    ggplot(aes(x = dateTime, y = Qls, group = site, color = site)) +
    geom_point() +
    geom_line() +
    ggtitle(paste0("ibw and subcatchments, storm:", stormNumber),
            subtitle = year(allSites[1,]$dateTime)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave(filename = paste0("flow_ibw_catchments_", stormNumber, ".png"),
         device = "png")
  
}


# generate plots for all storms
lapply(unique(contributingFlow[['stormMark']]), curry_subcatchments_discharge)


# quantify contributing subcatchments -------------------------------------

# The steps here detail calculating the reach length and side-channels that
# contribute to flow at Curry. Reach length 1:6 reflects the number of gauges
# from downstream (mckellips) to upstream (shea) for which there is flow at each
# gauge for a given storm (as identified by stormMark). For example, for a given
# stormMark, if there is flow at mckellips, indianschool, mcdonald, and shea,
# that storm would have a reach length of 3 (even though there was flow upstream
# of mcdonald @ shea, because that flow was not recorded at silverado, it is
# considered as not having contributed to the flow at Curry). granite reef,
# interceptor, lake marguerite, and berneil are considered side/contributing
# channels but not part of the main reach so are calculated independently and
# not factored into the reach length. A side/contributing channel is considered
# to have contributed (= 1) ONLY if all gauges below that channel contributed to
# flow at Curry.

# acquire contributingFlow (if not already loaded)
contributingFlow <- read_csv('~/Dropbox/SNAZ meeting materials/indianBendWash/results/contributing_flow/contributing_flow.csv') %>%
  arrange(stormMark, subcatchment)

# put the list of subcatchments for a given storm mark into a list
stormsList <- contributingFlow %>%
  group_by(stormMark) %>%
  summarise(
    subcatchmentList = list(subcatchment),
    subcatchmentStorms = list(subcatchmentStorms),
    cumQibw = round(max(cumQibw), digits = 0)
  ) %>% 
  ungroup() 


# identify nodes for reach length and contributing side channels
toMcKellips <- c('4603?dl=1')
toIndianSchool <- c('4603?dl=1',
                    '4618?dl=1')
toMcDonald <- c('4603?dl=1',
                '4618?dl=1',
                '4628?dl=1')
toSilverado <- c('4603?dl=1',
                 '4618?dl=1',
                 '4628?dl=1',
                 '4613?dl=1')
toShea <- c('4603?dl=1',
            '4618?dl=1',
            '4628?dl=1',
            '4613?dl=1',
            '4693?dl=1')
toSweetwater <- c('4603?dl=1',
                  '4618?dl=1',
                  '4628?dl=1',
                  '4613?dl=1',
                  '4693?dl=1',
                  '4643?dl=1')
fromGraniteReef <- c('4603?dl=1',
                     '4728?dl=1')
fromInterceptor <- c('4603?dl=1',
                     '4618?dl=1',
                     '4628?dl=1',
                     '4613?dl=1',
                     '4623?dl=1')
fromBerneil <- c('4603?dl=1',
                 '4618?dl=1',
                 '4628?dl=1',
                 '4613?dl=1',
                 '4688?dl=1')
fromLakeMarguerite <- c('4603?dl=1',
                        '4618?dl=1',
                        '4628?dl=1',
                        '4613?dl=1',
                        '4678?dl=1')


# identify reach length based on continuity (flow at each site along the chain)
# from downstream to upstream sites (mckellips:shea); identify when side
# channels (granite reef, interceptor, lake marguerite, and berneil) are
# contributing
stormsList$reachLength <- NA
stormsList$fromGraniteReef <- NA
stormsList$fromInterceptor <- NA
stormsList$fromBerneil <- NA
stormsList$fromLakeMarguerite <- NA
for (i in 1:nrow(stormsList)) {
  if (all(toMcKellips %in% unlist(stormsList[i,][['subcatchmentList']]))) { stormsList[i,]$reachLength = 1 }
  if (all(toIndianSchool %in% unlist(stormsList[i,][['subcatchmentList']]))) { stormsList[i,]$reachLength = 2 }
  if (all(toMcDonald %in% unlist(stormsList[i,][['subcatchmentList']]))) { stormsList[i,]$reachLength = 3 }
  if (all(toSilverado %in% unlist(stormsList[i,][['subcatchmentList']]))) { stormsList[i,]$reachLength = 4 }
  if (all(toShea %in% unlist(stormsList[i,][['subcatchmentList']]))) { stormsList[i,]$reachLength = 5 }
  if (all(toSweetwater %in% unlist(stormsList[i,][['subcatchmentList']]))) { stormsList[i,]$reachLength = 6 }
  if (all(fromGraniteReef %in% unlist(stormsList[i,][['subcatchmentList']]))) { stormsList[i,]$fromGraniteReef = 1 }
  if (all(fromInterceptor %in% unlist(stormsList[i,][['subcatchmentList']]))) { stormsList[i,]$fromInterceptor = 1 }
  if (all(fromBerneil %in% unlist(stormsList[i,][['subcatchmentList']]))) { stormsList[i,]$fromBerneil = 1 }
  if (all(fromLakeMarguerite %in% unlist(stormsList[i,][['subcatchmentList']]))) { stormsList[i,]$fromLakeMarguerite = 1 }
}

stormsList %>% 
  mutate(
    subcatchmentList = as.character(subcatchmentList),
    subcatchmentStorms = as.character(subcatchmentStorms)
  ) %>% 
  write_csv('~/Desktop/contributing_gauges.csv')


