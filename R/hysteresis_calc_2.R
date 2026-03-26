## Another script/way to calculate hysteresis based on python function
## takes processed cq data from merge_Q_chems.R

rm(list=ls())
library(here)
library(googledrive)
library(tidyverse)
library(reticulate)
library(utils)
library(purrr)

#### IMPORT DATA ####
# from google drive - originally from merge_Q_chems.R
curry_cq <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                             "1wWUPyysrln9gyWpfS6vAt2u-MS-LtOyT")) %>% 
  select(-X) %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"))
curry_cq[1,1] <- "2008-01-29 00:00:00"

silv_cq <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                             "1J-mW-MfwTfq7sOsy2sn4eejb9cq0ykvR")) %>% 
  select(-X) %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"))
silv_cq[1,1] <- "2008-01-29 00:00:00"

lakem_cq <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                            "100HPPcicy6tv7CUrwzDI8Yh4C6FBozhG")) %>% 
  select(-X) %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"))
lakem_cq[1,1] <- "2008-01-29 00:00:00"

#### Setting up python usage in R ####
#virtualenv_create("r-reticulate")
use_virtualenv("r-reticulate", required = TRUE)
if (!py_module_available("pandas")) {
  py_install("pandas", envname = "r-reticulate", pip = TRUE)
}

source_python("R/hysteresis_metrics.py")

reticulate::py_run_string("
import warnings
warnings.filterwarnings('ignore', category=RuntimeWarning)
")

#### HYSTERESIS PER STORM ####

# curry
curry_hysteresis <- curry_cq %>%
  group_by(stormID, analyte) %>%
  group_map(~ {
    q <- .x %>% 
      select(valuedatetime = datetime, datavalue = cfs) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))
    
    v <- .x %>% 
      select(valuedatetime = datetime, datavalue = mean_conc) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))
    
    if (nrow(v) < 2) return(NULL)
    
    q$valuedatetime <- as.POSIXct(q$valuedatetime, tz = "America/Phoenix")
    v$valuedatetime <- as.POSIXct(v$valuedatetime, tz = "America/Phoenix")
    
    hysteresisMetrics(r_to_py(q), r_to_py(v), 15L, 15L,
                      debug = FALSE,
                      interpall = TRUE,
                      discharge_time_spacing_units = "minutes",
                      response_time_spacing_units  = "minutes",
                      discharge_units = "CFS")
  }, .keep = TRUE)

# Name results as "stormID_analyte" for easy lookup
names(curry_hysteresis) <- curry_cq %>% 
  distinct(stormID, analyte) %>% 
  mutate(name = paste(stormID, analyte, sep = "_")) %>% 
  pull(name)

# Filter out incomplete results
curry_hysteresis <- curry_hysteresis[sapply(curry_hysteresis, function(x) {
  if (is.null(x)) return(FALSE)
  if (is.null(x$HI_count_and_interp)) return(FALSE)
  as.integer(x$HI_count_and_interp) > 0
})]


# silverado
silv_hysteresis <- silv_cq %>%
  group_by(stormID, analyte) %>%
  group_map(~ {
    q <- .x %>% 
      select(valuedatetime = datetime, datavalue = cfs) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))
    
    v <- .x %>% 
      select(valuedatetime = datetime, datavalue = mean_conc) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))
    
    if (nrow(v) < 2) return(NULL)
    
    q$valuedatetime <- as.POSIXct(q$valuedatetime, tz = "America/Phoenix")
    v$valuedatetime <- as.POSIXct(v$valuedatetime, tz = "America/Phoenix")
    
    hysteresisMetrics(r_to_py(q), r_to_py(v), 15L, 15L,
                      debug = FALSE,
                      interpall = TRUE,
                      discharge_time_spacing_units = "minutes",
                      response_time_spacing_units  = "minutes",
                      discharge_units = "CFS")
  }, .keep = TRUE)

# Name results as "stormID_analyte" for easy lookup
names(silv_hysteresis) <- silv_cq %>% 
  distinct(stormID, analyte) %>% 
  mutate(name = paste(stormID, analyte, sep = "_")) %>% 
  pull(name)

# Filter out incomplete results
silv_hysteresis <- silv_hysteresis[sapply(silv_hysteresis, function(x) {
  if (is.null(x)) return(FALSE)
  if (is.null(x$HI_count_and_interp)) return(FALSE)
  as.integer(x$HI_count_and_interp) > 0
})]

# Lake Marg.
lakem_hysteresis <- lakem_cq %>%
  group_by(stormID, analyte) %>%
  group_map(~ {
    q <- .x %>% 
      select(valuedatetime = datetime, datavalue = cfs) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))
    
    v <- .x %>% 
      select(valuedatetime = datetime, datavalue = mean_conc) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))
    
    if (nrow(v) < 2) return(NULL)
    
    q$valuedatetime <- as.POSIXct(q$valuedatetime, tz = "America/Phoenix")
    v$valuedatetime <- as.POSIXct(v$valuedatetime, tz = "America/Phoenix")
    
    hysteresisMetrics(r_to_py(q), r_to_py(v), 15L, 15L,
                      debug = FALSE,
                      interpall = TRUE,
                      discharge_time_spacing_units = "minutes",
                      response_time_spacing_units  = "minutes",
                      discharge_units = "CFS")
  }, .keep = TRUE)

# Name results as "stormID_analyte" for easy lookup
names(lakem_hysteresis) <- lakem_cq %>% 
  distinct(stormID, analyte) %>% 
  mutate(name = paste(stormID, analyte, sep = "_")) %>% 
  pull(name)

# Filter out incomplete results
lakem_hysteresis <- lakem_hysteresis[sapply(lakem_hysteresis, function(x) {
  if (is.null(x)) return(FALSE)
  if (is.null(x$HI_count_and_interp)) return(FALSE)
  as.integer(x$HI_count_and_interp) > 0
})]

# function to format each df
extract_hysteresis_dfs <- function(hysteresis_list, cq_data, site_name) {
  
  summary_df <- hysteresis_list %>%
    imap_dfr(~ {
      parts <- strsplit(.y, "_")[[1]]
      tibble(
        site             = site_name,
        stormID          = parts[1],
        analyte          = parts[2],
        HI_mean          = as.numeric(.x$HI_mean_with_Interp),
        HI_sd            = as.numeric(.x$HI_standard_deviation_with_Interp),
        HI_count         = as.integer(.x$HI_count_and_interp),
        normalized_slope = as.numeric(.x$`Normalized slope of response`),
        max_width        = as.numeric(.x$`interpolated Max width of response`),
        peak_Q           = as.numeric(.x$`Peak Q`)
      )
    })
  
  intervals_df <- hysteresis_list %>%
    imap_dfr(~ {
      parts <- strsplit(.y, "_")[[1]]
      imap_dfr(.x$Hysteresis_Index, ~ tibble(
        site     = site_name,
        stormID  = parts[1],
        analyte  = parts[2],
        interval = .y,
        HI_value = .x
      ))
    }) %>%
    mutate(interval_pct = as.numeric(gsub("Interpolated HI for |% discharge", "", interval)))
  
  storm_dates <- cq_data %>%
    group_by(stormID) %>%
    summarise(storm_date = min(datetime, na.rm = TRUE)) %>%
    mutate(stormID = as.character(stormID))
  
  summary_df <- summary_df %>%
    mutate(stormID = as.character(stormID)) %>%
    left_join(storm_dates, by = "stormID")
  
  list(summary = summary_df, intervals = intervals_df)
}

# apply function
curry_dfs <- extract_hysteresis_dfs(curry_hysteresis, curry_cq, "Curry")
silv_dfs  <- extract_hysteresis_dfs(silv_hysteresis,  silv_cq,  "Silverado")
lakem_dfs <- extract_hysteresis_dfs(lakem_hysteresis, lakem_cq, "Lake Margarita")

#combine into single dataframes across all sites
hysteresis_df    <- bind_rows(curry_dfs$summary,   silv_dfs$summary,   lakem_dfs$summary)
hi_intervals_df  <- bind_rows(curry_dfs$intervals, silv_dfs$intervals, lakem_dfs$intervals)

# export
saveRDS(curry_hysteresis, here("curry_hysteresis.rds"))
saveRDS(silv_hysteresis,  here("silv_hysteresis.rds"))
saveRDS(lakem_hysteresis, here("lakem_hysteresis.rds"))
write.csv(hysteresis_df,   here("hysteresis_summary.csv"),   row.names = FALSE)
write.csv(hi_intervals_df, here("hysteresis_intervals.csv"), row.names = FALSE)

drive_put(here("hysteresis_summary.csv"), path = as_id("1wG4zV1-Ekzt0qIsSpA-3BJsPpE7s86Vn")) 
drive_put(here("hysteresis_intervals.csv"), path = as_id("1wG4zV1-Ekzt0qIsSpA-3BJsPpE7s86Vn")) 
