# indian-bend-wash

repository for the analysis of Indian Bend Wash stormwater hydrology and chemistry

## Plan of (data) attack for Indian Bend Wash chemistry

### Weed out storms with poor coverage

* Use 50% as initial threshold of coverage, but visually check other hydrographs to see if they are large storms with still decent coverage over main pulse. Storms with > 50% of the storm covered by chem data include: 9, 10, 11, 14, 15, 16, 17, 29, 32, 33, 34, 37, 39, 42, 22, 67, 74 

* Questionable storms:
    + 25: 32% of storm covered, not much on falling limb, but good coverage of rising limb
    + 26: 67% of storm covered, but big gap between last two samples. Poor DOC coverage
    + 27: Good coverage during beginning and middle, but little analyte data during second half 
    + 28: 3 peaks in discharge, middle pulse well covered but not two other humps
    + 31: 2 pulses, first one is well covered, 2nd is not
    + 36: 34% of storm has analyte coverage, but looks like most of the storm volume is covered by samples
    + 75: two peaks in hydrograph, 2nd is covered, first is not
    + 89: another instance of two pulses, one is covered and one is not


### Calculate metrics for storm chemistry

* Flushing metric
    + characterizes dilution or enrichment/flushing
    + there seem to be several slightly different versions out there. One cited several times recently is presented well in Vaughan et al 2017 and looks at change in normalized concentration from onset of storm to the peak of the hydrograph
    + not sure of ref used previously but looks like the relationship is L'=Q'^b^, where L' is normalized load and Q is normalized discharge (normalized over total for storm event)
* Hysteresis metric
    + characterizes area and direction of hysteresis loop for normalized c-Q
    + see Vaughan et al 2017
* Total load
* Ratio of change in nutrient relative to conservative element?
* Others?


### Once we see some interesting differences between storm events

* look at sub-basin stream gages to start to get at where contributing area is
* consider getting high-res precip data with better coverage of contributing area & precip amounts
* decide on metrics to characterize storm events & their contributing area (land use, amount of precip, etc.)
    + Precip
    + Max discharge (normalized by basin area)
    + Basin area
    + Antecedent dry days
    + Season
    + Retention basin density
    + Pipe density
    + Grass cover
    + Bare soil cover
    + Impervious cover
    + Golf courses and/or parks

### Other questions

* do we want to use Jim Smith's precip data to explore thresholds where storm generation is actually occurring in parts of watershed?
* consider plotting up all c-Q data together for full record to see whether we're seeing chemostatic or chemodynamic patterns for nutrients vs geogenic elements (e.g., Ca) (see Basu et al 2010 paper for nice discussion of this)
