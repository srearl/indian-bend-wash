## Another script/way to calculate hysteresis based on python function
## takes processed cq data from merge_Q_chems.R

rm(list=ls())
library(here)
library(googledrive)
library(tidyverse)
library(reticulate)
library(utils)

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
  group_by(stormID) %>%
  group_map(~ {
    q <- .x %>% 
      select(valuedatetime = datetime, datavalue = cfs) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))  # drop NaT rows
    
    v <- .x %>% 
      select(valuedatetime = datetime, datavalue = mean_conc) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))  # drop NaT rows
    
    if (nrow(v) < 2) return(NULL)
    
    q$valuedatetime <- as.POSIXct(q$valuedatetime, tz = "American/Phoenix")
    v$valuedatetime <- as.POSIXct(v$valuedatetime, tz = "American/Phoenix")
    
    hysteresisMetrics(r_to_py(q), r_to_py(v), 15L, 15L,
                      debug = FALSE,
                      interpall = TRUE,
                      discharge_time_spacing_units = "minutes",
                      response_time_spacing_units  = "minutes",
                      discharge_units = "CFS")
  }, .keep = TRUE)

names(curry_hysteresis) <- unique(curry_cq$stormID)

curry_hysteresis <- curry_hysteresis[sapply(curry_hysteresis, function(x) {
  if (is.null(x)) return(FALSE)                          # remove NULLs
  if (is.null(x$HI_count_and_interp)) return(FALSE)     # remove empty runs like storm 1
  as.integer(x$HI_count_and_interp) > 0                 # keep only storms with HI values
})]

# silverado
silv_hysteresis <- silv_cq %>%
  group_by(stormID) %>%
  group_map(~ {
    q <- .x %>% 
      select(valuedatetime = datetime, datavalue = cfs) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))  
    
    v <- .x %>% 
      select(valuedatetime = datetime, datavalue = mean_conc) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))  
    
    if (nrow(v) < 2) return(NULL)
    
    q$valuedatetime <- as.POSIXct(q$valuedatetime, tz = "American/Phoenix")
    v$valuedatetime <- as.POSIXct(v$valuedatetime, tz = "American/Phoenix")
    
    hysteresisMetrics(r_to_py(q), r_to_py(v), 15L, 15L,
                      debug = FALSE,
                      interpall = TRUE,
                      discharge_time_spacing_units = "minutes",
                      response_time_spacing_units  = "minutes",
                      discharge_units = "CFS")
  }, .keep = TRUE)

names(silv_hysteresis) <- unique(silv_cq$stormID)

silv_hysteresis <- silv_hysteresis[sapply(silv_hysteresis, function(x) {
  if (is.null(x)) return(FALSE)                          
  if (is.null(x$HI_count_and_interp)) return(FALSE)     
  as.integer(x$HI_count_and_interp) > 0                 
})]


# Lake Marg.
lakem_hysteresis <- lakem_cq %>%
  group_by(stormID) %>%
  group_map(~ {
    q <- .x %>% 
      select(valuedatetime = datetime, datavalue = cfs) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))  
    
    v <- .x %>% 
      select(valuedatetime = datetime, datavalue = mean_conc) %>% 
      filter(!is.na(datavalue), !is.na(valuedatetime))  
    
    if (nrow(v) < 2) return(NULL)
    
    q$valuedatetime <- as.POSIXct(q$valuedatetime, tz = "American/Phoenix")
    v$valuedatetime <- as.POSIXct(v$valuedatetime, tz = "American/Phoenix")
    
    hysteresisMetrics(r_to_py(q), r_to_py(v), 15L, 15L,
                      debug = FALSE,
                      interpall = TRUE,
                      discharge_time_spacing_units = "minutes",
                      response_time_spacing_units  = "minutes",
                      discharge_units = "CFS")
  }, .keep = TRUE)

names(lakem_hysteresis) <- unique(lakem_cq$stormID)

lakem_hysteresis <- lakem_hysteresis[sapply(lakem_hysteresis, function(x) {
  if (is.null(x)) return(FALSE)                          
  if (is.null(x$HI_count_and_interp)) return(FALSE)     
  as.integer(x$HI_count_and_interp) > 0                 
})]
