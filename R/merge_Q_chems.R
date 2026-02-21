## Project: Indian Bend Wash
## Author: JCI
## Initialize Date: Feb 12 2026
## Purpose: To merge discharge data with chem data per site (Lake Margherite, Silverado, & Curry)
##    Inputs: 
##          Q data for Lake M and silverado from google drive (lakem_discharge.txt & silverado_discharge.txt)
##          Q data for Curry from USGS gauge **TODO: NEED TO GET**
##          Chems from google drive (624_runoff_chemistry.csv)
##    Outputs: 
##          file of compiled Q and chem data aligned by time per site ** might do file per site? **

#### LOAD LIBRARIES ####
rm(list=ls())
library(here)
library(googledrive)
library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(zoo)
library(xts)
library(imputeTS)


#### IMPORT DATA ####

## Q data
#drive_download("lakem_discharge.txt")
lakem_Q <- read.table(here("lakem_discharge.txt"), skip = 7)
names(lakem_Q) <- c("Date", "Time", "feet", "cfs")

#drive_download("silverado_discharge.txt")
silverado_Q <- read.table(here("silverado_discharge.txt"), skip = 7)
names(silverado_Q) <- c("Date", "Time", "feet", "cfs")
  
## Q from USGS for curry
curry_id <- "09512162"
discharge_cd <- "00060"

curry_Q <- readNWISuv(
  siteNumbers = curry_id,
  parameterCd = discharge_cd,
  startDate = "2018-01-11",
  endDate   = "2022-02-18"
)

curry_Q <- renameNWISColumns(curry_Q) 
curry_Q <-rename(curry_Q, curry_cfs=Flow_Inst)
curry_Q <-rename(curry_Q, datetime=dateTime)
curry_Q$datetime <- with_tz(curry_Q$datetime, tzone = "America/Denver")
curry_Q <- curry_Q %>% select(c(datetime, curry_cfs))

## Chem data
#drive_download("624_runoff_chemistry.csv")
chems <- read_csv("624_runoff_chemistry.csv")


#### DATA MUNGING ####
## rounding datetimes to 15mins, averaging discharge and chems every shared 15mins

lakem_Q$DateTime <- mdy_hms(
  paste(lakem_Q$Date, lakem_Q$Time),
  tz = "MST"
)

lakem_Q <- lakem_Q %>%
  mutate(datetime = round_date(DateTime, "15 minutes"))

lakem_Q <- lakem_Q %>%
  group_by(datetime) %>%
  summarise(
    lakem_cfs = mean(cfs, na.rm = TRUE),
    lakem_feet = mean(feet, na.rm = TRUE)
  ) %>%
  ungroup()


silverado_Q$DateTime <- mdy_hms(
  paste(silverado_Q$Date, silverado_Q$Time),
  tz = "MST"
)

silverado_Q <- silverado_Q %>%
  mutate(datetime = round_date(DateTime, "15 minutes"))

silverado_Q <- silverado_Q %>%
  group_by(datetime) %>%
  summarise(
    silv_cfs = mean(cfs, na.rm = TRUE),
    silv_feet = mean(feet, na.rm = TRUE)
  ) %>%
  ungroup()


chems <- chems %>%
  mutate(datetime = round_date(runoff_datetime, "15 minutes"))

chems_long <- chems %>%
  group_by(runoff_location, datetime, analysis_name) %>%
  summarise(
    mean_conc = mean(analysis_concentration, na.rm = TRUE),
    .groups = "drop"
  )

chems_wide <- chems %>%
  group_by(runoff_location, datetime, analysis_name) %>%
  summarise(
    mean_conc = mean(analysis_concentration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = analysis_name,
    values_from = mean_conc
  ) #wide would be better to merge with q


## Combine into one data frame, align by datetime

# datetime range according to chems : 2008-01-29 09:21:00 - 2022-02-23 13:33:00
start_datetime <- as.POSIXct("2008-01-29 00:00:00", tz = "America/Denver")
end_datetime <- as.POSIXct("2022-02-24 00:00:00", tz = "America/Denver")

datetime_seq <- seq(from = start_datetime,
                    to   = end_datetime,
                    by   = "15 mins")

q_all <- data.frame(datetime = datetime_seq)

q_all <- left_join(q_all, lakem_Q, by = "datetime")
q_all <- left_join(q_all, silverado_Q, by = "datetime")
q_all <- left_join(q_all, curry_Q, by = "datetime")

# interpolate discharge

q_all$datetime <- trimws(q_all$datetime)

parsed <- suppressWarnings(
  as.POSIXct(q_all$datetime,
             format="%Y-%m-%d %H:%M:%S",
             tz="America/Denver")
)

q_all <- q_all[!is.na(parsed), ]
q_all$datetime <- parsed[!is.na(parsed)]

q_xts <- xts(
  q_all[, sapply(q_all, is.numeric)],
  order.by = q_all$datetime
)

q_interp <- na_kalman(q_xts, model = "StructTS", type = "level")

q_final <- data.frame(
  datetime = index(q_interp),
  coredata(q_interp)
)

# visual check - before vs after
q_all %>% ggplot(aes(x = datetime, y = curry_cfs)) +
  geom_point()
q_final %>% ggplot(aes(x = datetime, y = curry_cfs)) +
  geom_point()
