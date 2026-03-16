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
library(hms)


#### IMPORT DATA ####

## Q data from q_storms.R - uploaded to drive
q_all <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", "1JB1nucoswpaxaAWuAXmhILEfD8awWiZ_")) %>% select(-X)
q_all[1,1] <- "2008-01-29 00:00:00"
q_all$datetime <- as.POSIXct(q_all$datetime , format = "%Y-%m-%d %H:%M:%S", tz = "America/Phoenix")
## Chem data
#drive_download("624_runoff_chemistry.csv", overwrite = TRUE)
chems <- read_csv("624_runoff_chemistry.csv")
chem_meta <- read_csv("624_analytes.csv")


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

write.csv(curry_cq, here("curry_cq.csv"))
drive_put(here("curry_cq.csv"), path = as_id("1wG4zV1-Ekzt0qIsSpA-3BJsPpE7s86Vn")) 

write.csv(silv_cq, here("silv_cq.csv"))
drive_put(here("silv_cq.csv"), path = as_id("1wG4zV1-Ekzt0qIsSpA-3BJsPpE7s86Vn")) 

write.csv(lakem_cq, here("lakem_cq.csv"))
drive_put(here("lakem_cq.csv"), path = as_id("1wG4zV1-Ekzt0qIsSpA-3BJsPpE7s86Vn")) 


#### PLOTTING ####

## attempting to make function for this - from sycburns
plot_c_q_norm <- function(cq_data, analyte_col) {#curry_cq = cq_data, "NO3" = analyte_col
  curr_site <- cq_data %>% 
    filter(analyte == analyte_col) %>% 
    filter( !is.na(cfs)) 
  
  if (nrow(curr_site) == 0) {
    message(paste("Skipping", site, "- no valid data"))
    return(NULL)
  }
  
  if (!"Time_hours" %in% names(curr_site)) {
    curr_site <- curr_site %>%
     # group_by(stormID) %>% 
      mutate(Time_hours = as.numeric(difftime(datetime, min(datetime), units = "hours")))
  }
  # # Remove top 10% of cfs values per Event_Date (storm)
  # curr_site <- curr_site %>%
  #   group_by(Event_Date) %>%
  #   filter(cfs <= quantile(cfs, 0.9, na.rm = TRUE)) %>%
  #   ungroup()
  
  #Normalize within each Event_Date (storm)
  curr_site <- curr_site %>%
    group_by(stormID) %>%
    mutate(
      Level_norm = (cfs - min(cfs, na.rm = TRUE)) /
        (max(cfs, na.rm = TRUE) - min(cfs, na.rm = TRUE)),
      Chem_norm = (.data$mean_conc - min(.data$mean_conc, na.rm = TRUE)) /
        (max(.data$mean_conc, na.rm = TRUE) - min(.data$mean_conc, na.rm = TRUE)),
      Time_norm = (Time_hours - min(Time_hours, na.rm = TRUE)) /
        (max(Time_hours, na.rm = TRUE) - min(Time_hours, na.rm = TRUE))
    ) %>%
    ungroup() %>%
    filter(!is.na(.data$mean_conc), !is.na(cfs)) 
  
  #Plot normalized C–Q relationship (per storm)
  p <- curr_site %>% filter(!is.na(curr_site$Level_norm), !is.na(curr_site$Chem_norm)) %>% ggplot(aes(x = Level_norm, y = Chem_norm)) + 
    geom_point(aes(color = Time_norm)) + 
    facet_wrap(~stormID, scales = "free") + 
    scale_color_gradientn(colours = rainbow(7)) + 
    labs( title = paste(curr_site$Site[1], "-", analyte_col), 
          x = "Normalized Stage (0–1)", 
          y = paste("Normalized", analyte_col, "(0–1)"), 
          color = "Time (within storm)" ) + 
    theme_minimal() 
  
  return(p)
}


# Loop through analytes per site to plot and save
setwd(here("plots", "cq"))

chem_names <- unique(curry_cq$analyte)
for(analyte in chem_names){
  if(!is.na(analyte)){
   p <- plot_c_q_norm(curry_cq, analyte)
   ggsave(p, file=paste("Curry_", analyte, ".jpeg"), width = 20, height = 10, units = "in")
  }
}

chem_names <- unique(silv_cq$analyte)
for(analyte in chem_names){
  if(!is.na(analyte)){
    p <- plot_c_q_norm(silv_cq, analyte)
    ggsave(p, file=paste("Silverado_", analyte, ".jpeg"), width = 20, height = 10, units = "in")
  }
}

chem_names <- unique(lakem_cq$analyte)
for(analyte in chem_names){
  if(!is.na(analyte)){
    p <- plot_c_q_norm(lakem_cq, analyte)
    ggsave(p, file=paste("LakeM_", analyte, ".jpeg"), width = 20, height = 10, units = "in")
  }
}


