
# README ------------------------------------------------------------------

# This workflow generates storm-analyte indices, including total load, b,
# cumulative Q, and emc, though additional indices, particularly related to
# hysteresis, can and should be included.

# Input to the worflow is a long-form data featuring flow and interpolated
# chemistry, here ibwQchem accessed from Dropbox is being used but any data with
# those characteristics could be used.

# libraries ---------------------------------------------------------------

library(tidyverse)
library(zoo)


# data import -------------------------------------------------------------

# import data from Dropbox, and remove analytes with a very small number of
# samples

ibwQchem <- read_csv('https://www.dropbox.com/s/4xv8q1pt6jpvkje/ibwQchem.csv?dl=1') %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  filter(!analyte %in% c('SO4D_IC', 'NiD_ICP', 'PbD_ICP', 'CaD_FLAME_AA', 'NO3D_IC'))
  # filter(stormMark == 39) # 39 was a practice data set


# identify storms with chemistry data -------------------------------------

storms_with_chem <- ibwQchem %>% 
  group_by(stormMark, analyte) %>%
  filter(any(!is.na(concentration))) %>%
  ungroup() %>%
  distinct(stormMark) %>%
  pull()


# calculate hydro metric(s) -----------------------------------------------

# calculations include:
  # totalLoad: total load of the analyte over the course of a storm
  # b: exponent of total load / cumulative Q power function
  # cumQ: cumulative discharge
  # emc: Event Mean Concentration

hydro_metrics <- ibwQchem %>% 
  filter(stormMark %in% c(storms_with_chem)) %>%
  group_by(stormMark, analyte) %>%
  filter(any(!is.na(concentration))) %>% 
  mutate(
    intpConc = na.approx(concentration, x = dateTime, na.rm = FALSE),
    intpConc = na.locf(intpConc, fromLast = TRUE, na.rm = FALSE),
    intpConc = na.locf(intpConc, na.rm = FALSE),
    load = (intpConc * Qls * (60/1000000)),
    normLoad = cumsum(load)/max(cumsum(load))
  ) %>% 
  summarise(
    totalLoad = sum(load),
    b = coef(nls(cumsum(load)/max(cumsum(load)) ~ I((cumQ/max(cumQ))^b), start=list(b=1), trace = F)),
    cumQ = max(cumQ),
    emc = (sum(load)/max(cumQ))*1000000
  )


