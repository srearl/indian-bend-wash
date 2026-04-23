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


hourly_cols <- c(
  "year", "doy", "hour",
  "air_temp", "rel_humidity", "vpd", "solar_rad", "precip",
  "soil_temp_4in", "soil_temp_20in",
  "wind_speed_avg", "wind_vec_mag", "wind_vec_dir", "wind_dir_sd", "wind_speed_max",
  "eto",
  "actual_vapor_pressure", "dewpoint"
)

UCR_new <- drive_get(as_id("https://drive.google.com/drive/folders/1wP4iKeIlEfKU_tWOV8zc8Lau8cqAlNrq"))
UCR_glist <- drive_ls(UCR_new, type = "txt")

setwd(here("Data/precip"))
walk(UCR_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))

file_list <- list.files(recursive = FALSE, pattern = "\\.txt$", full.names = TRUE)

ppt_raw <- map_dfr(
  file_list,
  ~ read.csv(.x, header = FALSE, col.names = hourly_cols)
)

setwd(here())

