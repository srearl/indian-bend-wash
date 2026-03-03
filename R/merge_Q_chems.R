## Project: Indian Bend Wash
## Author: JCI
## Initialize Date: Feb 12 2026
## Purpose: To merge discharge data with chem data per site (Lake Margherite, Silverado, & Curry)
##    Inputs: 
##          Q data for Lake M and silverado from google drive (lakem_discharge.txt & silverado_discharge.txt)
##          Q data for Curry from USGS gauge - code provided for online download
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


chems <- chems %>% filter(runoff_location %in% c("IBW", "SGC","LM")) %>% 
  mutate(datetime = round_date(runoff_datetime, "15 minutes"))

chems$runoff_location[chems$runoff_location == "IBW"] <- "curry"
chems$runoff_location[chems$runoff_location == "SGC"] <- "silverado"
chems$runoff_location[chems$runoff_location == "LM"] <- "lakem"

chems <- rename(chems, Site = runoff_location)

chems_long <- chems %>%
  group_by(Site, datetime, analysis_name) %>%
  summarise(
    mean_conc = mean(analysis_concentration, na.rm = TRUE),
    .groups = "drop"
  )

chems_wide <- chems %>%
  group_by(Site, datetime, analysis_name) %>%
  summarise(
    mean_conc = mean(analysis_concentration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = analysis_name,
    values_from = mean_conc
  ) #just another option


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

q_interp <- read_csv(here("discharge_interpolated.csv")) %>% select(-'...1')

#### interpolate discharge - commenting out and just reloading the interpolated file to save time####

# q_all$datetime <- as.POSIXct(q_all$datetime, tz = "America/Denver")
# 
# # Convert to xts
# q_xts <- xts(
#   q_all[, -1],
#   order.by = q_all$datetime
# )
# 
# # Interpolate column-wise (convert to numeric first)
# q_interp_xts <- q_xts
# 
# for (i in 1:ncol(q_xts)) {
# 
#   x <- as.numeric(q_xts[, i])
# 
#   q_interp_xts[, i] <- na_interpolation(
#     x,
#     option = "linear",
#   )
# }
# 
# # Convert back to dataframe
# q_interp <- data.frame(
#   datetime = index(q_interp_xts),
#   coredata(q_interp_xts)
# )
# 
# #check - should be 0 NAs
# colSums(is.na(q_interp))

# visual check - before vs after
# q_all %>% ggplot(aes(x = datetime, y = silv_cfs)) +
#   geom_point()
# q_interp %>% ggplot(aes(x = datetime, y = silv_cfs)) +
#   geom_point()

#export
#write.csv(q_interp, here("discharge_interpolated.csv"))



#### MERGE CHEMS AND Q ####
q_interp <- rename(q_interp, lakem = lakem_cfs)
q_interp <- rename(q_interp, curry = curry_cfs)
q_interp <- rename(q_interp, silverado = silv_cfs)
q_interp <- q_interp %>% select(c(datetime, lakem, curry, silverado))

#TODO: assign storm events - unless Stevan knows where that info already is

q_long <- q_interp %>% pivot_longer(cols =c(lakem, curry, silverado), names_to = "Site", 
                                    values_to = "q_cfs")

q_long$datetime     <- force_tz(q_long$datetime, "America/Phoenix")
chems_long$datetime <- force_tz(chems_long$datetime, "America/Phoenix")

q_chems <- inner_join(q_long, chems_long, by = c("datetime", "Site")) #inner join = where we have discharge AND chems

#### PLOTTING ####
#Plot time series for Q per site
(curry.q.pl <- q_interp %>% #filter(Site == "curry") %>% 
  ggplot(aes(x = datetime, y = curry)) +
  geom_point(position = position_jitter(width = 0.15), size = 1.5) +
  #facet_wrap(~ analysis_name, scales = "free_y") +
  labs(y = "Q (cubic square feet)", title="Curry discharge ") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = 10),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15))
)

ggsave(curry.q.pl, path = "plots", file = "curry_discharge.jpeg", width = 10, height = 7.5, units = "in")

(silv.q.pl <- q_interp %>% #filter(Site == "curry") %>% 
    ggplot(aes(x = datetime, y = silverado)) +
    geom_point(position = position_jitter(width = 0.15), size = 1.5) +
    #facet_wrap(~ analysis_name, scales = "free_y") +
    labs(y = "Q (cubic square feet)", title="Silverado discharge ") + 
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, size = 10),
          strip.text = element_text(size = 15),
          legend.text = element_text(size = 15))
)

ggsave(silv.q.pl, path = "plots", file = "silverado_discharge.jpeg", width = 10, height = 7.5, units = "in")

(lakem.q.pl <- q_interp %>% #filter(Site == "curry") %>% 
    ggplot(aes(x = datetime, y = lakem)) +
    geom_point(position = position_jitter(width = 0.15), size = 1.5) +
    #facet_wrap(~ analysis_name, scales = "free_y") +
    labs(y = "Q (cubic square feet)", title="Lake Marg. discharge ") + 
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, size = 10),
          strip.text = element_text(size = 15),
          legend.text = element_text(size = 15))
)

ggsave(lakem.q.pl, path = "plots", file = "lake_marg_discharge.jpeg", width = 10, height = 7.5, units = "in")


# plotting timeseries of chems per site

(lakem.chems.pl <- chems_long %>% filter(Site == "lakem") %>% 
    ggplot(aes(x = datetime, y = mean_conc)) +
    geom_point(position = position_jitter(width = 0.15), size = 1.5) +
    facet_wrap(~ analysis_name, scales = "free_y") +
    labs(y = "mean concentraiton", title="Lake Marg. Chems ") + 
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, size = 10),
          strip.text = element_text(size = 15),
          legend.text = element_text(size = 15))
)

ggsave(lakem.chems.pl, path = "plots", file = "lake_marg_chems.jpeg", width = 10, height = 7.5, units = "in")

(curry.chems.pl <- chems_long %>% filter(Site == "curry") %>% 
    ggplot(aes(x = datetime, y = mean_conc)) +
    geom_point(position = position_jitter(width = 0.15), size = 1.5) +
    facet_wrap(~ analysis_name, scales = "free_y") +
    labs(y = "mean concentration", title="Curry chems ") + 
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, size = 10),
          strip.text = element_text(size = 15),
          legend.text = element_text(size = 15))
)

ggsave(curry.chems.pl, path = "plots", file = "curry_chems.jpeg", width = 10, height = 7.5, units = "in")

(silv.chems.pl <- chems_long %>% filter(Site == "silverado") %>% 
    ggplot(aes(x = datetime, y = mean_conc)) +
    geom_point(position = position_jitter(width = 0.15), size = 1.5) +
    facet_wrap(~ analysis_name, scales = "free_y") +
    labs(y = "mean concentration", title="Silverado chems ") + 
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, size = 10),
          strip.text = element_text(size = 15),
          legend.text = element_text(size = 15))
)

ggsave(silv.chems.pl, path = "plots", file = "silverado_chems.jpeg", width = 10, height = 7.5, units = "in")

(curry.cq.full.pl <- q_chems %>% filter(Site == "curry") %>% 
    ggplot(aes(x = q_cfs, y = mean_conc)) +
    geom_point(position = position_jitter(width = 0.15), size = 1.5) +
    facet_wrap(~ analysis_name, scales = "free_y") +
    labs(y = "Mean concentration", title="Curry CQ full dataset ") + 
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, size = 10),
          strip.text = element_text(size = 15),
          legend.text = element_text(size = 15))
)
ggsave(curry.cq.full.pl, path = "plots", file = "curry_full_cq.jpeg", width = 10, height = 7.5, units = "in")

(lakem.cq.full.pl <- q_chems %>% filter(Site == "lakem") %>% 
    ggplot(aes(x = q_cfs, y = mean_conc)) +
    geom_point(position = position_jitter(width = 0.15), size = 1.5) +
    facet_wrap(~ analysis_name, scales = "free_y") +
    labs(y = "Mean concentration", title="Lake Marg. CQ full dataset ") + 
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, size = 10),
          strip.text = element_text(size = 15),
          legend.text = element_text(size = 15))
)
ggsave(lakem.cq.full.pl, path = "plots", file = "lakem_full_cq.jpeg", width = 10, height = 7.5, units = "in")

(silv.cq.full.pl <- q_chems %>% filter(Site == "silverado") %>% 
    ggplot(aes(x = q_cfs, y = mean_conc)) +
    geom_point(position = position_jitter(width = 0.15), size = 1.5) +
    facet_wrap(~ analysis_name, scales = "free_y") +
    labs(y = "Mean concentration", title="Silverado CQ full dataset ") + 
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, size = 10),
          strip.text = element_text(size = 15),
          legend.text = element_text(size = 15))
)
ggsave(silv.cq.full.pl, path = "plots", file = "silv_full_cq.jpeg", width = 10, height = 7.5, units = "in")
