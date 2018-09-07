
# README ------------------------------------------------------------------

# This workflow is to facilitate a visual assessment of normalized discharge
# versus normalized concentration sensu Vaughan et al. 2017.

# Vaughan, M. C. H., et al. (2017), High‐frequency dissolved organic carbon and
# nitrate measurements reveal differences in storm hysteresis and loading in
# relation to land cover and seasonality, Water Resour. Res., 53, 5345–5363,
# doi: 10.1002/2017WR020491.

# Input to the worflow is long-form data featuring flow and interpolated
# chemistry, here ibwQchem accessed from Dropbox (with additional processing in
# import_from_file.R) is being used but any data with those characteristics
# could be used.


# source ------------------------------------------------------------------

# source hysteresis_calcs.R for requisite input to plotting functions

source("hysteresis_calcs.R")


# plotting: Qnorm vs. Qconc through time ----------------------------------

plot_hysteresis <- function(storm) {
  
  singleStorm <- normalizedQchem %>% 
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


# plotting: dateTime vs. Qnorm with limbs ---------------------------------

# plot dateTime vs. Qnorm with rising & falling limbs demarcated
normalizedQchem %>%   
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
  
  singleStorm <- normalizedQchem %>% 
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

