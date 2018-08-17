
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


# optons ------------------------------------------------------------------

options(scipen=999)


# data import -------------------------------------------------------------

# import data from Dropbox, and remove analytes with a very small number of
# samples

# note specifying the time zone on import, which is critical to maintaining the
# integrity of these data as readr will otherwise convert dates/times to UTC
# without warning nor shifting the corresponding data accordingly

ibwQchem <- read_csv('https://www.dropbox.com/s/wsseakmze4hsnws/ibwQchem.csv?dl=1',
                     locale = locale(tz = "America/Phoenix")) %>%  # note !!!
  mutate(concentration = as.numeric(concentration)) %>% 
  filter(!analyte %in% c('SO4D_IC', 'NiD_ICP', 'PbD_ICP', 'CaD_FLAME_AA', 'NO3D_IC'))


# identify storms with chemistry data -------------------------------------

storms_with_chem <- ibwQchem %>% 
  group_by(stormMark, analyte) %>%
  filter(any(!is.na(concentration))) %>%
  ungroup() %>%
  distinct(stormMark) %>%
  pull()


# calculate hydro metric(s) -----------------------------------------------

# calculations include:
  # stormDuration: duration of storm in hours
  # totalLoad: total load of the analyte over the course of a storm
  # b: exponent of total load / cumulative Q power function
  # cumQ: cumulative discharge
  # maxQ: maximum discharge
  # emc: Event Mean Concentration
  # maxC: maximum concentration

hydro_metrics <- inner_join(
  ibwQchem %>% 
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
      stormDuration = round(difftime(max(dateTime), min(dateTime), units = c("hours")), digits = 1),
      totalLoad = round(sum(load), digits = 0),
      b = round(coef(nls(cumsum(load)/max(cumsum(load)) ~ I((cumQ/max(cumQ))^b), start=list(b=1), trace = F)), digits = 3),
      cumQ = round(max(cumQ), digits = 0),
      maxQ = round(max(Qls), digits = 0),
      emc = round((sum(load)/max(cumQ))*1000000, digits = 2),
      maxC = round(max(concentration, na.rm = TRUE), digits = 2)
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
    chemCoverage = (chemDuration / stormDuration) * 100,
    sampleDensity = (numSamples / stormDuration)
  )