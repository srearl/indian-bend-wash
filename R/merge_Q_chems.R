## Project: Indian Bend Wash
## Author: JCI
## Initialize Date: Feb 12 2026
## Purpose: To merge discharge data with chem data per site (Lake Margherite, Silverado, & Curry)
##    Inputs: 
##          Discharge data, interpolated and storms assigned, from q_storms.R
##          Chems from google drive (624_runoff_chemistry.csv)
##    Outputs: 
##          file of compiled Q and chem data aligned by time per site 
##          CQ plots per storm per analyte per site

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
library(hms)


#### IMPORT DATA ####

## Q data from q_storms.R - uploaded to drive
q_all <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", "1JB1nucoswpaxaAWuAXmhILEfD8awWiZ_")) %>% select(-X)
q_all[1,1] <- "2008-01-29 00:00:00"
q_all$datetime <- as.POSIXct(q_all$datetime , format = "%Y-%m-%d %H:%M:%S", tz = "America/Phoenix")
## Chem data
#drive_download("624_runoff_chemistry.csv", overwrite = TRUE)
chems <- read_csv("Data/624_runoff_chemistry.csv")
chem_meta <- read_csv("Data/624_analytes.csv")


#### DATA MUNGING ####

chems <- chems %>% filter(runoff_location %in% c("IBW", "SGC","LM")) %>% 
  mutate(datetime = round_date(runoff_datetime, "15 minutes"))

chems$runoff_location[chems$runoff_location == "IBW"] <- "curry"
chems$runoff_location[chems$runoff_location == "SGC"] <- "silverado"
chems$runoff_location[chems$runoff_location == "LM"] <- "lakem"

chems <- rename(chems, Site = runoff_location)


#toss the NO3 IC and Ca Varian
chems <-chems %>%  filter(analysis_name != "CaD_FLAME_AA", 
                analysis_name != "NO3D_IC")
chem_meta <- rename(chem_meta, analysis_name=analysis)

chems_meta <- left_join(chems, chem_meta, by ="analysis_name")



chems_long <- chems_meta %>%
  group_by(Site, datetime, analyte, true_unit) %>%
  summarise(
    mean_conc = mean(analysis_concentration, na.rm = TRUE),
    .groups = "drop"
  )

chems_wide <- chems_meta %>%
  group_by(Site, datetime, analysis_name) %>%
  summarise(
    mean_conc = mean(analysis_concentration, na.rm = TRUE),
    standard_dev = sd(analysis_concentration, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = analysis_name,
    values_from = mean_conc
  ) #just another option


#### MERGE CHEMS AND Q ####
# do by site to make it easier
q_all$Time <- as.POSIXct(format(q_all$datetime,"2000-01-01 %H:%M:%S"))
curry_q <- q_all %>% select(c(datetime, Time, curry_cfs, curry_start, curry_storm)) 
names(curry_q) <- c("datetime", "Time", "cfs", "storm_start", "stormID")
curry_q$Time <- as_hms(curry_q$Time)

silv_q <- q_all %>% select(c(datetime, Time,silv_cfs, silv_start, silv_storm)) 
names(silv_q) <-  c("datetime", "Time", "cfs", "storm_start", "stormID")

lakem_q <- q_all %>% select(c(datetime, Time,lakem_cfs, lakem_start, lakem_storm)) 
names(lakem_q) <-  c("datetime", "Time", "cfs", "storm_start", "stormID")

curry_cq <- left_join(curry_q, chems_long %>% filter(Site == "curry"), by = "datetime")
curry_cq$Site <- "Curry"
silv_cq <- left_join(silv_q, chems_long %>% filter(Site == "silverado"), by = "datetime")
silv_cq$Site <- "Silverado"
lakem_cq <- left_join(lakem_q, chems_long %>% filter(Site == "lakem", analyte != "Ni", analyte != "Pb"), by = "datetime")
lakem_cq$Site <- "Lake Marg"


## TODO: remove outliers, concentration > 3*SD for the storm
test <- curry_cq %>% group_by(stormID, analyte) %>% 
  mutate(outlier = ifelse(mean_conc > 3*standard_dev, "yes", "no"))

## export
write.csv(curry_cq, here("Data/curry_cq.csv"))
drive_put(here("Data/curry_cq.csv"), path = as_id("1wG4zV1-Ekzt0qIsSpA-3BJsPpE7s86Vn")) 

write.csv(silv_cq, here("Data/silv_cq.csv"))
drive_put(here("Data/silv_cq.csv"), path = as_id("1wG4zV1-Ekzt0qIsSpA-3BJsPpE7s86Vn")) 

write.csv(lakem_cq, here("Data/lakem_cq.csv"))
drive_put(here("Data/lakem_cq.csv"), path = as_id("1wG4zV1-Ekzt0qIsSpA-3BJsPpE7s86Vn")) 




