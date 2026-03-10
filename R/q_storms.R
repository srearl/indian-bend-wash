## Project: Indian Bend Wash
## Author: JCI
## Initialize Date: March 10 2026
## Purpose: To import discharge from Lake Margherite, Silverado, & Curry, delineate unique storms, and interpolate Q
##    Inputs: 
##          Q data for Lake M and silverado from google drive (lakem_discharge.txt & silverado_discharge.txt)
##          Q data for Curry from USGS gauge - code provided for online download
##    Outputs: 
##          file of compiled Q (all sites in 1) with storm marks and 15min intervals with no gaps

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

## Q data
#drive_download("lakem_discharge.txt")
lakem_Q <- read.table(here("lakem_discharge.txt"), skip = 7)
names(lakem_Q) <- c("Date", "Time", "feet", "cfs")

#drive_download("silverado_discharge.txt")
silverado_Q <- read.table(here("silverado_discharge.txt"), skip = 7)
names(silverado_Q) <- c("Date", "Time", "feet", "cfs")

## Q from USGS for curry

curry_Q <- readNWISuv(
  siteNumbers = "09512162",
  parameterCd = "00060",
  startDate = "2008-01-01",
  endDate   = "2022-02-18"
)

curry_Q <- renameNWISColumns(curry_Q) 
curry_Q <-rename(curry_Q, datetime=dateTime)
curry_Q$datetime <- with_tz(curry_Q$datetime, tzone = "America/Phoenix")
curry_Q <- curry_Q %>% select(c(datetime, Flow_Inst))
curry_Q <-rename(curry_Q, curry_cfs=Flow_Inst)


## rounding datetimes to 15mins, averaging discharge and chems every shared 15mins

lakem_Q$DateTime <- mdy_hms(
  paste(lakem_Q$Date, lakem_Q$Time),
  tz = "America/Phoenix"
)

lakem_Q <- lakem_Q %>%
  mutate(datetime = round_date(DateTime, "15 minutes"))

lakem_Q <- lakem_Q %>%
  group_by(datetime) %>%
  summarise(
    cfs = mean(cfs, na.rm = TRUE)
  ) %>%
  ungroup()
lakem_Q <-rename(lakem_Q, lakem_cfs=cfs)


silverado_Q$DateTime <- mdy_hms(
  paste(silverado_Q$Date, silverado_Q$Time),
  tz = "America/Phoenix"
)

silverado_Q <- silverado_Q %>%
  mutate(datetime = round_date(DateTime, "15 minutes"))

silverado_Q <- silverado_Q %>%
  group_by(datetime) %>%
  summarise(
    cfs = mean(cfs, na.rm = TRUE)
  ) %>%
  ungroup()
silverado_Q <-rename(silverado_Q, silv_cfs=cfs)

## Combine into one data frame, align by datetime

#datetime range according to chems : 2008-01-29 09:21:00 - 2022-02-23 13:33:00
start_datetime <- as.POSIXct("2008-01-29 00:00:00", tz = "America/Phoenix")
end_datetime <- as.POSIXct("2022-02-24 00:00:00", tz = "America/Phoenix")

datetime_seq <- seq(from = start_datetime,
                    to   = end_datetime,
                    by   = "15 mins")

q_all <- data.frame(datetime = datetime_seq)

q_all <- left_join(q_all, lakem_Q, by = "datetime")
q_all <- left_join(q_all, silverado_Q, by = "datetime")
q_all <- left_join(q_all, curry_Q, by = "datetime")

#### Interpolate discharge ####
q_all$datetime <- as.POSIXct(q_all$datetime, tz = "America/Phoenix")

# Convert to xts
q_xts <- xts(
  q_all[, -1],
  order.by = q_all$datetime
)

# Interpolate column-wise (convert to numeric first)
q_interp_xts <- q_xts

for (i in 1:ncol(q_xts)) {
  
  x <- as.numeric(q_xts[, i])
  
  q_interp_xts[, i] <- na_interpolation(
    x,
    option = "linear",
  )
}

# Convert back to dataframe
q_interp <- data.frame(
  datetime = index(q_interp_xts),
  coredata(q_interp_xts)
)

colSums(is.na(q_interp))#should be 0 NAs


#### Storm delineation #### 

#original code for storm delineation is from ibw_flow_delin_interp.R

assign_storm <- function(df, threshold = 0, dry_steps = 24){
  
  df <- df %>% arrange(datetime)
  
  dry <- df$cfs <= threshold
  
  # count consecutive dry steps
  dry_count <- ave(dry, cumsum(!dry), FUN = seq_along)
  dry_count[!dry] <- 0
  
  # storm starts when flow rises above threshold after sufficient dryness
  storm_start <- df$cfs > threshold &
    dplyr::lag(dry_count, default = 0) >= dry_steps
  
  # handle dataset starting mid-storm
  storm_start[1] <- df$cfs[1] > threshold
  
  df %>%
    mutate(
      storm_start = storm_start,
      storm_id = cumsum(storm_start)
    )
}


curry_interp <- q_interp %>% select(datetime, curry_cfs) %>% rename(cfs = curry_cfs)
curry_q_storm <- assign_storm(curry_interp)
curry_q_storm <- rename(curry_q_storm, curry_cfs = cfs, curry_storm = storm_id,  curry_start = storm_start)
curry_q_storm %>% ggplot(aes(x = datetime, y = cfs, color = as.factor(storm_id)))+
  geom_point() + labs(title= "Curry storms")


silv_interp <- q_interp %>% select(datetime, silv_cfs) %>% rename(cfs = silv_cfs)
silverado_Q_storm <- assign_storm(silv_interp)
silverado_Q_storm <- rename(silverado_Q_storm, silv_cfs = cfs, silv_storm = storm_id, silv_start = storm_start)
silverado_Q_storm %>% filter(silv_cfs>100, silv_cfs < 2000) %>% ggplot(aes(x = datetime, y = silv_cfs, color = as.factor(silv_storm)))+
  geom_point() + labs(title= "Silverado storms")


lakem_interp <- q_interp %>% select(datetime, lakem_cfs) %>% rename(cfs = lakem_cfs)
lakem_Q_storm <- assign_storm(lakem_interp)
lakem_Q_storm <- rename(lakem_Q_storm, lakem_cfs = cfs, lakem_storm = storm_id,  lakem_start = storm_start)
lakem_Q_storm %>% filter(lakem_cfs>100, lakem_cfs < 2000) %>% ggplot(aes(x = datetime, y = lakem_cfs, color = as.factor(lakem_storm)))+
  geom_point() + labs(title= "Lake Marg. storms")

q_all_storm <- full_join(curry_q_storm, silverado_Q_storm, by = "datetime")
q_all_storm <- full_join(q_all_storm, lakem_Q_storm, by = "datetime")


write.csv(q_all_storm, here("q_all_storms.csv"))#ignored file on git repo
drive_put(here("q_all_storms.csv"), path = as_id("1YMfuWZDBvBRK_puKliPpHA3rahWtdcpR"))
