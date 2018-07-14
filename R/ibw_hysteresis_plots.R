
# README ------------------------------------------------------------------

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
  )


# plot function -----------------------------------------------------------

plot_hysteresis <- function(storm) {
  
  singleStorm <- ibwQchem %>% 
    filter(stormMark == storm)
  
  ggplot(singleStorm, aes(x = Qnorm, y = Cnorm)) +
    geom_point(aes(colour = dateTime)) +
    scale_colour_gradient(low = "green", high = "red", guide = FALSE) +
    facet_wrap(~ analyte) +
    ggtitle(paste0("storm: ", storm))
  
  fileName <- paste0("ibw_hysteresis_", storm, ".png")
  
  ggsave(filename = fileName,
         device = "png")
  
}


# generate plots for relevant storms --------------------------------------

lapply(storms_with_chem, plot_hysteresis)
