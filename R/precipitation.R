## Project: Indian Bend Wash
## Author: JCI
## Initialize Date: April 16 2026
## Purpose: To import and munge precip data for IBW
##    Inputs: 
##          daily precip data from https://www.maricopa.gov/625/Rainfall-Data - 56000 Osborn and 64th ave station
##    Outputs: 
##         

library(tidyverse)
library(here)
library(googledrive)
library(googlesheets4)
library(lubridate)
library(rio)



sheet_url <- "https://docs.google.com/spreadsheets/d/1tZYh73C8ww8MvTLauS1M_seQ2R5_HkWRPSi3Tze2euM"
sheet_names <- sheet_names(sheet_url)

precip <- lapply(sheet_names, function(sheet) {
  df <- read_sheet(
    sheet_url, 
    sheet = sheet, 
    col_names = FALSE,   # don't treat first row as header
    col_types = "c"      # read all as character
  )
  # keep only first two columns, rename them
  df <- df[, 1:2]
  names(df) <- c("date", "precip_mm")
  df$year <- sheet
  return(df)
}) |> bind_rows()

# clean up types and drop any NA rows
precip <- precip |>
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    precip_mm = as.numeric(precip_mm)
  ) |>
  filter(!is.na(date), !is.na(precip_mm))


precip %>% ggplot(aes(x = date, y = precip_mm))+
  geom_col() +theme_bw() +labs( title = "Daily IBW Precipitation", subtitle = "Data from Maricopa County", 
                                     y = "Precipitation (mm)", x = "Date")

write.csv(precip, here("IBW_daily_precip.csv"))

