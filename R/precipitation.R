## Project: Indian Bend Wash
## Author: JCI
## Initialize Date: April 16 2026
## Purpose: To import and munge precip data for IBW
##    Inputs: 
##          daily precip data from https://www.maricopa.gov/625/Rainfall-Data - 56000 Osborn and 64th ave station
##    Outputs: 
##         

rm(list=ls())
library(tidyverse)
library(here)
library(googledrive)
library(googlesheets4)
library(lubridate)
library(rio)
library(zoo)


## Uncomment lines below to download raw data for the first time
# hourly_cols <- c(
#   "year", "doy", "hour",
#   "air_temp", "rel_humidity", "vpd", "solar_rad", "precip",
#   "soil_temp_4in", "soil_temp_20in",
#   "wind_speed_avg", "wind_vec_mag", "wind_vec_dir", "wind_dir_sd", "wind_speed_max",
#   "eto",
#   "actual_vapor_pressure", "dewpoint"
# )
# 
# UCR_new <- drive_get(as_id("https://drive.google.com/drive/folders/1wP4iKeIlEfKU_tWOV8zc8Lau8cqAlNrq"))
# UCR_glist <- drive_ls(UCR_new, type = "txt")
# 
# setwd(here("Data/precip"))
# walk(UCR_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# 
# file_list <- list.files(recursive = FALSE, pattern = "\\.txt$", full.names = TRUE)
# 
# ppt_raw <- map_dfr(
#   file_list,
#   ~ read.csv(.x, header = FALSE, col.names = hourly_cols)
# )
# 
# setwd(here())
# 
# write.csv(ppt_raw, here("Data/precip", "ppt_raw.csv"))

ppt_raw <- read_csv(here("Data/precip", "ppt_raw.csv")) %>% select(-`...1`)

#### CALCULATE METRICS ####
## cumulative ppt in rolling windows of various durations before each storm sample (1, 3 months, week)

ppt_all <- ppt_raw %>% select(c(year, doy, hour, precip)) %>% 
  group_by(year, doy) %>%
  mutate(daily_tot = sum(precip)) 


ppt_roll <- ppt_all %>%
  group_by(year) %>%
  mutate(ppt_1month_cum = rollapply(daily_tot, width = 30, align = "right", FUN = sum, fill = NA, na.rm = TRUE),
         ppt_1month_avg = rollapply(daily_tot, width = 30, align = "right", FUN = mean, fill = NA, na.rm = TRUE),
         ppt_1week_cum = rollapply(daily_tot, width = 7, align = "right", FUN = sum, fill = NA, na.rm = TRUE),
         ppt_1week_avg = rollapply(daily_tot, width = 7, align = "right", FUN = mean, fill = NA, na.rm = TRUE),
         ppt_3month_cum = rollapply(daily_tot, width = 90, align = "right", FUN = sum, fill = NA, na.rm = TRUE),
         ppt_3month_avg = rollapply(daily_tot, width = 90, align = "right", FUN = mean, fill = NA, na.rm = TRUE)) %>%
  ungroup()

## calculate storm size, storm intensity
# storm delination - original code from q_storms.R

assign_storm <- function(df, threshold = 0, dry_steps = 24){
  
  #df <- df %>% arrange(datetime)
  
  dry <- df$precip <= threshold
  
  # count consecutive dry steps
  dry_count <- ave(dry, cumsum(!dry), FUN = seq_along)
  dry_count[!dry] <- 0
  
  # storm starts when flow rises above threshold after sufficient dryness
  storm_start <- df$precip > threshold &
    dplyr::lag(dry_count, default = 0) >= dry_steps
  
  # handle dataset starting mid-storm
  storm_start[1] <- df$precip[1] > threshold
  
  df %>%
    mutate(
      storm_start = storm_start,
      storm_id = cumsum(storm_start)
    )
}

ppt_storms <- assign_storm(ppt_roll)

# storm size - total precip per storm, storm intensity = cumulative/duration of storm
ppt_storms <- ppt_storms %>% group_by(storm_id) %>% mutate(storm_size = sum(precip), 
                                                           storm_duration_hr = row_number(),
                                                           storm_intensity = storm_size / max(storm_duration_hr), 
                                                           Date = as.Date(doy, origin = "2002-12-31"), 
                                                           datetime = as.POSIXct(paste(year, doy, hour),format = "%Y %j %H",tz = "UTC"), 
                                                           season = ifelse((month(Date) >= 10 | month(Date) < 5),  "winter", "summer")
                                                           )

# export
write.csv(ppt_storms, here("Data/precip", "ibw_precip.csv"))


#### PLOTTING ####

ppt_storms %>% ggplot(aes(x = datetime, y = precip, color = season))+
  geom_col() +
  labs(title = "Hourly Precip") +
  facet_wrap(~year, scales = "free") + theme_classic()

ppt_storms %>% ggplot(aes(x = datetime, y = daily_tot, color = season))+
  geom_col() +
  labs(title = "Daily Precip", y = "precip (mm)") +
  facet_wrap(~year, scales = "free") + theme_classic()

