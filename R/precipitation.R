## Project: Indian Bend Wash
## Author: JCI
## Initialize Date: April 16 2026
## Purpose: To import and munge precip data for IBW
##    Inputs: 
##          daily precip data from AZMet desert ridge station
##          q_all.csv from q_storms.R
##    Outputs: 
##         

# rm(list=ls())
# library(tidyverse)
# library(here)
# library(googledrive)
# library(googlesheets4)
# library(lubridate)
# library(rio)
# library(zoo)
# library(xts)
# library(imputeTS)
# library(slider)
# 
# 
# # Uncomment lines below to download raw data for the first time
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
  mutate(Date = as.Date(doy, origin = "2002-12-31"), 
         datetime = as.POSIXct(paste(year, doy, hour),format = "%Y %j %H",tz = "UTC"))

ppt_all <- ppt_all %>% select(c(datetime, precip))

#interpolate to 15min intervals - interpolation code from q_storms.R
start_datetime <- as.POSIXct("2008-01-29 00:00:00", tz = "America/Phoenix")
end_datetime <- as.POSIXct("2026-02-24 00:00:00", tz = "America/Phoenix")

datetime_seq <- seq(from = start_datetime,
                    to   = end_datetime,
                    by   = "15 mins")

ppt_seq <- data.frame(datetime = datetime_seq)

ppt_seq <- left_join(ppt_seq, ppt_all, by = "datetime")

#interpolate
ppt_seq$datetime <- as.POSIXct(ppt_seq$datetime, tz = "America/Phoenix")

# Convert to xts
ppt_xts <- xts(
  ppt_seq[, -1],
  order.by = ppt_seq$datetime
)

# Interpolate column-wise (convert to numeric first)
ppt_interp_xts <- ppt_xts

for (i in 1:ncol(ppt_xts)) {
  
  x <- as.numeric(ppt_xts[, i])
  
  ppt_interp_xts[, i] <- na_interpolation(
    x,
    option = "linear",
  )
}

# Convert back to dataframe
ppt_interp <- data.frame(
  datetime = index(ppt_interp_xts),
  coredata(ppt_interp_xts)
)

colSums(is.na(ppt_interp))#should be 0 NAs
names(ppt_interp) <- c("datetime", "precip_mm")

#### calculate storm metrics ####
# need storm timestamps from q_storms.R
q_all <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", "1JB1nucoswpaxaAWuAXmhILEfD8awWiZ_")) %>% select(-X)
q_all[1,1] <- "2008-01-29 00:00:00"
q_all$datetime <- as.POSIXct(q_all$datetime , format = "%Y-%m-%d %H:%M:%S", tz = "America/Phoenix")

# storm size - total precip per storm, storm intensity = cumulative/duration of storm

q_storms_precip <- left_join(q_all, ppt_interp, by = "datetime")

curry <- q_storms_precip %>% select(c(datetime, curry_cfs, curry_start, curry_storm, precip_mm))
silv <- q_storms_precip %>% select(c(datetime, silv_cfs, silv_start, silv_storm, precip_mm))
lakem <- q_storms_precip %>% select(c(datetime, lakem_cfs, lakem_start, lakem_storm, precip_mm))

curry <- curry %>% group_by(curry_storm) %>% mutate(curry_storm_size = sum(precip_mm), 
                                                           curry_storm_duration_hr = row_number(),
                                                           curry_storm_intensity = curry_storm_size / max(curry_storm_duration_hr) 
                                                           #curry_season = ifelse((month(datetime) >= 10 | month(datetime) < 5),  "winter", "summer")
)

silv <- silv %>% group_by(silv_storm) %>% mutate(silv_storm_size = sum(precip_mm), 
                                                    silv_storm_duration_hr = row_number(),
                                                    silv_storm_intensity = silv_storm_size / max(silv_storm_duration_hr)
)

lakem <- lakem %>% group_by(lakem_storm) %>% mutate(lakem_storm_size = sum(precip_mm), 
                                                    lakem_storm_duration_hr = row_number(),
                                                    lakem_storm_intensity = lakem_storm_size / max(lakem_storm_duration_hr)
)

# calc pre-storm preicp
curry_calc <- curry %>%
  ungroup() %>%                     # critical: don't compute rolling sums per storm group
  arrange(datetime) %>%
  filter(!is.na(datetime))          # or fix upstream if these should exist

curry_calc <- curry_calc %>%
  mutate(
    curry_precip_7d_mm  = slide_index_dbl(precip_mm, datetime, ~sum(.x, na.rm = TRUE),
                                    .before = days(7),  .after = -minutes(15)),
    curry_precip_30d_mm = slide_index_dbl(precip_mm, datetime, ~sum(.x, na.rm = TRUE),
                                    .before = days(30), .after = -minutes(15)),
    curry_precip_90d_mm = slide_index_dbl(precip_mm, datetime, ~sum(.x, na.rm = TRUE),
                                    .before = days(90), .after = -minutes(15))
  )

curry_calc <- curry_calc %>%
  filter(curry_start) %>%
  select(curry_storm,  curry_precip_7d_mm, curry_precip_30d_mm, curry_precip_90d_mm)

curry_precip <- left_join(curry, curry_calc, by = "curry_storm")

silv_calc <- silv %>%
  ungroup() %>%                     # critical: don't compute rolling sums per storm group
  arrange(datetime) %>%
  filter(!is.na(datetime))          # or fix upstream if these should exist

silv_calc <- silv_calc %>%
  mutate(
    silv_precip_7d_mm  = slide_index_dbl(precip_mm, datetime, ~sum(.x, na.rm = TRUE),
                                    .before = days(7),  .after = -minutes(15)),
    silv_precip_30d_mm = slide_index_dbl(precip_mm, datetime, ~sum(.x, na.rm = TRUE),
                                    .before = days(30), .after = -minutes(15)),
    silv_precip_90d_mm = slide_index_dbl(precip_mm, datetime, ~sum(.x, na.rm = TRUE),
                                    .before = days(90), .after = -minutes(15))
  )

silv_calc <- silv_calc %>%
  filter(silv_start) %>%
  select(silv_storm,  silv_precip_7d_mm, silv_precip_30d_mm, silv_precip_90d_mm)

silv_precip <- left_join(silv, silv_calc, by = "silv_storm")

lakem_calc <- lakem %>%
  ungroup() %>%                     # critical: don't compute rolling sums per storm group
  arrange(datetime) %>%
  filter(!is.na(datetime))          # or fix upstream if these should exist

lakem_calc <- lakem_calc %>%
  mutate(
    lakem_precip_7d_mm  = slide_index_dbl(precip_mm, datetime, ~sum(.x, na.rm = TRUE),
                                         .before = days(7),  .after = -minutes(15)),
    lakem_precip_30d_mm = slide_index_dbl(precip_mm, datetime, ~sum(.x, na.rm = TRUE),
                                         .before = days(30), .after = -minutes(15)),
    lakem_precip_90d_mm = slide_index_dbl(precip_mm, datetime, ~sum(.x, na.rm = TRUE),
                                         .before = days(90), .after = -minutes(15))
  )

lakem_calc <- lakem_calc %>%
  filter(lakem_start) %>%
  select(lakem_storm,  lakem_precip_7d_mm, lakem_precip_30d_mm, lakem_precip_90d_mm)

lakem_precip <- left_join(lakem, lakem_calc, by = "lakem_storm")
#todo: rejoin and export
q_storms_precip_calc <- left_join(q_storms_precip, curry_calc, by = "curry_storm")
q_storms_precip_calc <- left_join(q_storms_precip_calc, silv_calc, by = "silv_storm")
q_storms_precip_calc <- left_join(q_storms_precip_calc, lakem_calc, by = "lakem_storm")


# export
write.csv(q_storms_precip_calc, here("Data", "q_storms_precip.csv"))
drive_put(here("Data/q_storms_precip.csv"), path = as_id("1D5bHhP6_egHu-rkCTCGFI3E2auyxIbfS"))


#### PLOTTING ####
#need to update these - replace ppt_storms with q_storms_precip
q_storms_precip_calc %>% ggplot(aes(x = datetime, y = precip_mm, color = season))+
  geom_col() +
  labs(title = "Hourly Precip") +
  facet_wrap(~year(datet), scales = "free") + theme_classic()

ppt_storms %>% ggplot(aes(x = datetime, y = daily_tot, color = season))+
  geom_col() +
  labs(title = "Daily Precip", y = "precip (mm)") +
  facet_wrap(~year, scales = "free") + theme_classic()

