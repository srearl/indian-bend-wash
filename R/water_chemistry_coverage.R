
# README ------------------------------------------------------------------

# This workflow is intended to generally assess the portion of a hydrograph for
# which there is corresponding chemistry data, and the number of samples per the
# duration of the hydgraph - an attempt to identify storms with sufficient
# chemistry data for analysis.

# Input to the worflow is a long-form data featuring flow and interpolated
# chemistry, here ibwQchem accessed from Dropbox is being used but any data with
# those characteristics could be used.


# libraries ---------------------------------------------------------------

library(tidyverse)
library(zoo)


# optons ------------------------------------------------------------------

options(scipen = 999)


# data import -------------------------------------------------------------

# moved to import_from_file.R


# identify storms with chemistry data -------------------------------------

storms_with_chem <- ibwQchem %>% 
  group_by(stormMark, analyte) %>%
  filter(any(!is.na(concentration))) %>%
  ungroup() %>%
  distinct(stormMark) %>%
  pull()


# water chemistry coveage -------------------------------------------------

inner_join(
  ibwQchem %>% 
    filter(stormMark %in% c(storms_with_chem)) %>%
    group_by(stormMark, analyte) %>%
    filter(any(!is.na(concentration))) %>% 
    summarise(
      stormDuration = round(difftime(max(dateTime), min(dateTime), units = c("hours")), digits = 1)
    ) %>% ungroup(),
  ibwQchem %>% 
    filter(
      stormMark %in% c(storms_with_chem),
      !is.na(concentration)
    ) %>%
    group_by(stormMark, analyte) %>%
    summarise(
      chemDuration = round(difftime(max(dateTime), min(dateTime), units = c("hours")), digits = 1),
      numSamples = n()
    ) %>% 
    ungroup(),
  by = c("stormMark", "analyte")
) %>% 
  mutate(
    chemCoverage = round((as.numeric(chemDuration, units = "hours") / as.numeric(stormDuration, units = "hours") * 100), digits = 1),
    sampleDensity = round((numSamples / as.numeric(stormDuration, units = "hours")), digits = 1)
  ) %>% # stop here for summary by storm & analyte, continue for summary by storm 
  group_by(stormMark) %>% 
  summarise(
    temporalCoverage = mean(chemCoverage),
    density = mean(sampleDensity)
  )