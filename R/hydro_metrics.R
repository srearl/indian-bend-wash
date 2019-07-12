
# README ------------------------------------------------------------------

# This workflow generates storm-analyte indices, including total load, b,
# cumulative Q, and emc, though additional indices, particularly related to
# hysteresis, can and should be included.

# Input to the worflow is a long-form data featuring flow and interpolated
# chemistry, here ibwQchem accessed from Dropbox is being used but any data with
# those characteristics could be used.


# libraries (in addition to TV from import_from_file.R) -------------------

library(zoo)
library(lubridate)
library(here)


# options -----------------------------------------------------------------

options(scipen = 999)


# data import -------------------------------------------------------------

if (!exists("ibwQchem") | !exists("ibwQminute")) {
  source(here::here("R", "import_from_file.R"))
}


# assess months in which storms occur -------------------------------------

# distribution of months in which storms with good chem. coverage occur for
# assigning a season index; no June or Oct storms in this set so set monsoon (1)
# to storms in July, August, and September. For exploration only, uncomemnt to
# rerun.

# ibwQchem %>% 
#   filter(stormMark %in% c(storms_with_chem)) %>%
#   distinct(dateTime) %>% 
#   mutate(month = month(dateTime)) %>% 
#   ggplot(aes(x = month)) +
#   geom_histogram(binwidth = 1) +
#   scale_x_continuous(breaks = c(1:12))


# calculate hydro metric(s) -----------------------------------------------

# calculations include:
  # stormDuration: duration of storm in hours
  # totalLoad: total load of the analyte over the course of a storm
  # b: exponent of total load / cumulative Q power function
  # cumQ: cumulative discharge
  # maxQ: maximum discharge
  # emc: Event Mean Concentration
  # maxC: maximum concentration
  # monsoon: whether storm occurs during monsoon season (1 = mos. 7-9)
  # antecedentDay: antecedent dry day

hydroMetrics <- inner_join(
  ibwQchem %>% 
    filter(stormMark %in% c(storms_with_chem)) %>%
    group_by(stormMark, analyte) %>%
    filter(any(!is.na(concentration))) %>% 
    mutate(
      intpConc = na.approx(concentration, x = dateTime, na.rm = FALSE),
      intpConc = na.locf(intpConc, fromLast = TRUE, na.rm = FALSE),
      intpConc = na.locf(intpConc, na.rm = FALSE),
      load = (intpConc * Qls * (60/1000000)),
      normLoad = cumsum(load)/max(cumsum(load)),
      monsoon = case_when(
        month(dateTime) %in% c(7,8,9) ~ 1,
        TRUE ~ 0
      )
    ) %>% 
    summarise(
      stormDuration = difftime(max(dateTime), min(dateTime), units = c("hours")),
      totalLoad = round(sum(load), digits = 0),
      b = round(coef(nls(cumsum(load)/max(cumsum(load)) ~ I((cumQ/max(cumQ))^b), start=list(b=1), trace = F)), digits = 3),
      cumQ = round(max(cumQ), digits = 0),
      maxQ = round(max(Qls), digits = 0),
      emc = round((sum(load)/max(cumQ))*1000000, digits = 2),
      maxC = round(max(concentration, na.rm = TRUE), digits = 2),
      monsoon = max(monsoon)
    ) %>%
    ungroup(),
  ibwQminute %>%
    group_by(stormMark) %>%
    summarise(
      minDT = min(dateTime),
      maxDT = max(dateTime)
    ) %>%
    ungroup() %>%
    mutate(antecedentDay = difftime(minDT, lag(maxDT, n = 1L), units = c("days"))) %>% 
    select(stormMark, antecedentDay),
  by = c("stormMark")) %>% 
  mutate(
    monsoon = as.factor(monsoon),
    stormDuration = round(as.numeric(stormDuration), digits = 1),
    antecedentDay = round(as.numeric(antecedentDay), digits = 2)
  )
