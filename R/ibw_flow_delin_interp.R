
# README ------------------------------------------------------------------

# Workflow is to harvest discharge data form the USGS gauge on IBW at Curry,
# delineate unique storms, interpolate discharge to one-minute intervals, and
# marry one-minute Q and IBW chemistry data. Outputs are the interpolated
# one-minute discharge data, and the married Q and chemsitry data. Be so, so
# CAREFUL of dates and times. A tremendous error occurred with earlier
# iterations of these data owing to tidyverse automatically converting read and
# write dates to UTC.

# libraries ---------------------------------------------------------------

library(dataRetrieval)
library(tidyverse)
library(zoo)
library(RPostgreSQL)


# settings ----------------------------------------------------------------

options(scipen = 999) # too much sci. notation with these data


# database connection -----------------------------------------------------

source('~/Documents/localSettings/pg_prod.R')
pg <- pg_prod


# harvest data from USGS --------------------------------------------------

ibwQ <- readNWISuv('09512162', '00060', startDate = "2008-01-01", endDate = Sys.Date(), tz = 'America/Phoenix') %>% 
  rename(
    cfs = X_00060_00000,
    QCflag = X_00060_00000_cd
  ) %>% 
  filter(QCflag != "P") %>% # filter provisisional data


# storm delineation -------------------------------------------------------

# delinate distinct storms; loop needs to be run multiple times untl the row
# count does not change - surely there is a better way to do this
i <- 1
for (i in 1:nrow(ibwQ)-1) {
  ifelse(ibwQ[i-1,]$cfs == 0 && ibwQ[i,]$cfs == 0 && ibwQ[i+1,]$cfs == 0, ibwQ <- ibwQ[-i,], ibwQ <- ibwQ)
}

# omit the first and last rows, which are not caught by the for loop
ibwQ <- ibwQ[-1,] # omit first row leftover from loop
ibwQ <- ibwQ[-nrow(ibwQ),] # omit last row leftover from loop

# add a field for marking the start of storms
ibwQ <- ibwQ %>%
  mutate(stormMark = NA)

# mark the start of all new storms with a 1
for (i in 2:nrow(ibwQ)) {
  if(ibwQ[i-1,]$cfs == 0 & ibwQ[i,]$cfs == 0) {
    ibwQ[i,]$stormMark = 1
  }
}

# create unique storm marks of sequential values
k <- 0
for (i in 1:nrow(ibwQ)) {
  if(!is.na(ibwQ[i,]$stormMark)) {
    ibwQ[i,]$stormMark = ibwQ[i,]$stormMark + k
    k = k + 1
    }
}

# populate all rows with corresponding stormMark (initialize first storm = 0)
ibwQ[1,]$stormMark <- 0 
ibwQ <- ibwQ  %>%
  mutate(stormMark = zoo::na.locf(stormMark))


# interpolate to one-minute intervals -------------------------------------

# create one min-data for each storm
one_min_q <- data.frame(one_min = NA)

for (i in unique(ibwQ$stormMark)) {
  firstDate <- min(ibwQ[ibwQ$stormMark == i,]$dateTime)
  lastDate <- max(ibwQ[ibwQ$stormMark == i,]$dateTime)
  framename <- paste0("storm", i)
  framename <- data.frame(one_min = seq(firstDate, lastDate, by = "min"))
  framename$stormMark <- i
  one_min_q <- bind_rows(one_min_q, framename)
}

one_min_q <- one_min_q %>% filter(!is.na(one_min))

# merge existing data with one min values
ibwQminute <- right_join(ibwQ, one_min_q, by = c("dateTime" = "one_min", "stormMark" = "stormMark"))

# cfs to Ls; interpolate Q; calc cumulative Q
ibwQminute <- ibwQminute %>%
  mutate(Qls = cfs * 28.316847) %>% # calc Q as Lsec
  group_by(stormMark) %>%
  mutate(
    Qls = na.approx(Qls), # interpolate Q between readings
    cumQ = cumsum(Qls)*60 # cumulative discharge
  ) %>% 
  select(stormMark, dateTime, Qls, cumQ) %>% 
  ungroup()

# change dates to character else write_csv will convert them to UTC (or use
# write.csv); see this issue: https://github.com/tidyverse/readr/issues/743)
ibwQminute %>% 
  mutate(dateTime = as.character(dateTime)) %>% 
  write_csv('~/Desktop/ibwQminute.csv')

# marry discharge and chemistry -------------------------------------------

# get chem data from the database
ibwChem <- dbGetQuery(pg,'
SELECT
  s.bottle,
  s.sample_datetime,
  r.replicate,
  a.analysis_name,
  r.concentration,
  r.data_qualifier,
  r.comments
FROM stormwater.results r
JOIN stormwater.samples s ON (r.sample_id = s.sample_id)
JOIN stormwater.analysis a ON (r.analysis_id = a.analysis_id)
WHERE
  s.site_id = 11 AND
  EXTRACT (YEAR FROM s.sample_datetime) >= 2008
ORDER BY s.sample_datetime, a.analysis_name;')

# filter chem data to remove blanks and suspect icp data in august 2012;
# group_by is to address duplicates, here taking the mean when there are more
# than one sample; no need to filter blanks by time as all were captured by the
# grepl for "blk"
ibwChem <- ibwChem %>%
  filter(
    !grepl("questionable", comments, ignore.case = T), # filter aug 2012 questionable icp data
    !grepl("blk", bottle, ignore.case = T) # filter blanks
  ) %>% 
  group_by(sample_datetime, analysis_name) %>%
  summarise(avg_conc = mean(concentration, na.rm = TRUE)) %>% 
  ungroup()

# because we are going to merge chem and Q data by date, confirm that time zones
# match and adjust if necessary (here need to adjust ibwChem) - necessary as the
# USGS data has a timezone
attr(ibwQminute$dateTime, "tzone")
attr(ibwChem$sample_datetime, "tzone")
attr(ibwChem$sample_datetime, "tzone") <- "America/Phoenix"

# join Q and chem on spread chem data
ibwChemSpread <- ibwChem %>%
  spread(analysis_name, avg_conc)

ibwQchemWide <- left_join(ibwQminute, ibwChemSpread, by = c("dateTime" = "sample_datetime"))

# back to long for working data
ibwQchem <- ibwQchemWide %>% 
  gather(key = analyte, value = concentration, CaD_FLAME_AA:ZnD_ICP)

# change dates to character else write_csv will convert them to UTC (or use
# write.csv); see this issue: https://github.com/tidyverse/readr/issues/743)
ibwQchem %>% 
  mutate(dateTime = as.character(dateTime)) %>% 
  write_csv('~/Desktop/ibwQchem.csv')
