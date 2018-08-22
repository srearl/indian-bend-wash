
# README ------------------------------------------------------------------

# UPDATE 2018-08-17: Workflow expanded to include delineation of rising and
# falling limbs, and plotting with limb identity. Hysteresis metrics, including
# a HI grand mean (HImean) as the mean of HIs computed for quartiles of Qnorm
# for each storm/analyte, and a flushing index (FI) as Cnorm at Qnorm-max -
# Cnorm at the outset of the storm (note this equivalent to slope as rise over
# run, where run is 1 for Qnorm). Workflow may feature all storms with chemistry
# or those storms with the best (> 50% coverage) as identified by Katie - make
# this distinction in the data import section.

# This workflow is to facilitate a visual assessment of normalized discharge
# versus normalized concentration sensu Vaughan et al. 2017.

# Vaughan, M. C. H., et al. (2017), High‐frequency dissolved organic carbon and
# nitrate measurements reveal differences in storm hysteresis and loading in
# relation to land cover and seasonality, Water Resour. Res., 53, 5345–5363,
# doi: 10.1002/2017WR020491.

# Input to the worflow is a long-form data featuring flow and interpolated
# chemistry, here ibwQchem accessed from Dropbox is being used but any data with
# those characteristics could be used.


# libraries ---------------------------------------------------------------

library(tidyverse)
library(zoo)
library(lubridate)


# options -----------------------------------------------------------------

options(scipen = 999)


# data import -------------------------------------------------------------

# moved to import_from_file.R


# subset to relevant storms -----------------------------------------------

# moved to import_from_file.R


# calculate hydro and runoff metrics --------------------------------------

ibwQchem <- ibwQchem %>% 
  filter(stormMark %in% c(storms_with_chem)) %>%
  group_by(stormMark, analyte) %>%
  filter(any(!is.na(concentration))) %>%
  mutate(
    intpConc = na.approx(concentration, x = dateTime, na.rm = FALSE),
    intpConc = na.locf(intpConc, fromLast = TRUE, na.rm = FALSE),
    intpConc = na.locf(intpConc, na.rm = FALSE),
    Qnorm = (Qls - min(Qls)) / (max(Qls) - min(Qls)),
    Cnorm = (intpConc - min(intpConc)) / (max(intpConc) - min(intpConc))
  ) %>% 
  ungroup()


# plotting: Qnorm vs. Qconc through time ----------------------------------

plot_hysteresis <- function(storm) {
  
  singleStorm <- ibwQchem %>% 
    filter(stormMark == storm)
  
  ggplot(singleStorm, aes(x = Qnorm, y = Cnorm)) +
    geom_point(aes(colour = as.numeric(dateTime))) +
    scale_colour_gradient(low = "green", high = "red", guide = FALSE) +
    facet_wrap(~ analyte) +
    ggtitle(paste0("storm: ", storm))
  
  fileName <- paste0("ibw_hysteresis_", storm, ".png")
  
  ggsave(filename = fileName,
         device = "png")
  
}

# generate plots for relevant storms

lapply(storms_with_chem, plot_hysteresis)


# identify rising and falling limbs ---------------------------------------

# get dateTimes that bracket the beginning and end of the period of max Qnorm
# throughout the hydrograph; using a bracket as there is or can be a range, not
# just one time step, featuring the maximum Qnorm for the storm
bracketQnorm <- ibwQchem %>% 
  group_by(stormMark) %>%
  filter(Qnorm == max(Qnorm)) %>% 
  summarise(
    minDateTime = min(dateTime),
    maxDateTime = max(dateTime)
  ) %>% 
  ungroup() %>% 
  gather(key = "max_min", value = "maxMinDate", -stormMark) %>% 
  arrange(stormMark, maxMinDate)

# if the minimum and maximum bounds of the maximum Qnorm are the same time
# (i.e., not a range but in fact the same point), that will produce problems in
# our processing later. To avoid that, we will add a fudge by subtracting
# one-minute from the minumum bound of the period of max Qnorm.
bracketQnorm[bracketQnorm$max_min == "minDateTime",]$maxMinDate <- bracketQnorm[bracketQnorm$max_min == "minDateTime",]$maxMinDate - minutes(1)

# join the max Qnorm boundaries to the discharge record (from data import)
minuteMinMaxQnorm <- ibwQminute %>% 
  filter(stormMark %in% storms_with_chem) %>% 
  left_join(bracketQnorm[,c("max_min", "maxMinDate")], by = c("dateTime" = "maxMinDate"))

# assign rising (0) and falling (1) limb designations to all points in the
# hydrgraph (except between the min and max bounds when Qnorm is a range)

minuteMinMaxQnorm$limb <- NA

for (i in unique(minuteMinMaxQnorm$stormMark)) {
  
  minDate <- minuteMinMaxQnorm[minuteMinMaxQnorm$stormMark == i & grepl("min", minuteMinMaxQnorm$max_min),]$dateTime
  maxDate <- minuteMinMaxQnorm[minuteMinMaxQnorm$stormMark == i & grepl("max", minuteMinMaxQnorm$max_min),]$dateTime
  
  minuteMinMaxQnorm[minuteMinMaxQnorm$stormMark == i & minuteMinMaxQnorm$dateTime <= minDate,]$limb <- 0
  minuteMinMaxQnorm[minuteMinMaxQnorm$stormMark == i & minuteMinMaxQnorm$dateTime >= maxDate,]$limb <- 1
  
}

# join minuteMinMaxQnorm such that we have a rising and falling limb designation
# for all combinations of storms analytes.
ibwQchem <- ibwQchem %>% 
  inner_join(minuteMinMaxQnorm[c("dateTime", "limb")], by = c("dateTime")) %>% 
  mutate(limb = as.factor(limb))


# plotting: dateTime vs. Qnorm with limbs ---------------------------------

# plot dateTime vs. Qnorm with rising & falling limbs demarcated
ibwQchem %>%   
mutate(limb = as.factor(limb)) %>% 
  ggplot(aes(x = dateTime, y = Qnorm, color = limb)) +
  geom_point() +
  facet_wrap(~ stormMark, scales = "free") +
  ggtitle("normalized discharge, and rising and falling limbs",
          subtitle = "IBW storms with > 50% coverage") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# plotting: Qnorm vs. Qconc rising and falling limbs ----------------------

# Function to plot Qnorm vs. Qconc with points delineated as the rising or
# falling limb (or in between). These are identical to previously constructed
# plots using the plot_hysteresis function except that the point color reflects
# limb state rather than time from start to end of the storm
plot_hysteresis_limbs <- function(storm) {
  
  singleStorm <- ibwQchem %>% 
    filter(stormMark == storm)
  
  ggplot(singleStorm, aes(x = Qnorm, y = Cnorm)) +
    geom_point(aes(colour = limb)) +
    facet_wrap(~ analyte) +
    ggtitle(paste0("storm: ", storm))
  
  fileName <- paste0("ibw_hysteresis_", storm, ".png")
  
  ggsave(filename = fileName,
         device = "png")
  
}


# generate plots for relevant storms
lapply(storms_with_chem, plot_hysteresis_limbs)


# hysteresis metrics ------------------------------------------------------

# hysteresis index

# calculate mean Cnorm in 25% quartiles of Qnorm
normalizedQuartiles <- ibwQchem %>%
  group_by(stormMark, analyte) %>%
  mutate(
    qnormQuartile = case_when(
      Qnorm < 0.25 ~ "Q1",
      Qnorm >= 0.25 & Qnorm < 0.50 ~ "Q2",
      Qnorm >= 0.50 & Qnorm < 0.75 ~ "Q3",
      Qnorm >= 0.75 & Qnorm <= 1.00 ~ "Q4"
    )
  ) %>% 
  ungroup() %>% 
  filter(!is.na(limb)) %>% 
  group_by(stormMark, analyte, limb, qnormQuartile) %>% 
  summarise(meanCnorm = mean(Cnorm)) %>% 
  ungroup()
  
  
# calculate a single mean HI from mean HI in quartiles
hysteresisIndices <- inner_join(
  normalizedQuartiles %>% 
    select(stormMark, analyte, limb, qnormQuartile, meanCnorm) %>% 
    filter(limb == 0),
  normalizedQuartiles %>% 
    select(stormMark, analyte, limb, qnormQuartile, meanCnorm) %>% 
    filter(limb == 1),
  by = c("stormMark", "analyte", "qnormQuartile"),
  suffix = c("R", "F")
) %>% 
  mutate(HIquart = meanCnormR - meanCnormF) %>% # stop here for HI by quartile
  select(stormMark, analyte, qnormQuartile, HIquart) %>% 
  group_by(stormMark, analyte) %>% 
  summarise(HImean = mean(HIquart)) %>% 
  ungroup()

# flusing index
flushingIndex <- inner_join(
  ibwQchem %>% 
    group_by(stormMark, analyte) %>%
    filter(Qnorm == max(Qnorm)) %>%
    summarise(CQpeak = max(Cnorm)), # max value here but what about cases like storm 9 where Cnorm @ Qnorm == 1 is all over the place
  ibwQchem %>% 
    filter(limb == 0) %>%
    group_by(stormMark, analyte) %>%
    slice(1L) %>% 
    select(stormMark, analyte, CQinit = Cnorm),
  by = c("stormMark", "analyte")) %>% 
  ungroup() %>% 
  mutate(FI = CQpeak - CQinit)

# include flusing in hysteresis indices
hysteresisIndices <- hysteresisIndices %>% 
  inner_join(flushingIndex, by = c("stormMark", "analyte"))