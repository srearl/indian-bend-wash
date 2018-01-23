
# README ------------------------------------------------------------------

# This workflow is to facilitate a visual assessment of analyte density and
# concentration during a storm hydrograph with a general goal of determining
# which analyte/storm combinations are suitable for including in the project
# analyses. A dataframe titled ibwQchem with variables including stormMark,
# dateTime, Qls, analyte, and concentration is required.


# libraries ---------------------------------------------------------------

library(tidyverse)


# data processing ---------------------------------------------------------

# stormChemPlot is a function to plot analyte concentration overlaid with Q for
# a discrete storm. Inputs include storm (as a storm mark) and chem (as the
# analyte of interest). For analytes without concentration data only the
# hydrograph is plotted.
stormChemPlot <- function(storm, chem) {
  
  ibwQchem <- ibwQchem[ibwQchem$stormMark == storm,]
  
  # only plot Q vs. date time if there are not chem data
  if(nrow(ibwQchem %>% filter(analyte == chem) %>% filter(!is.na(concentration))) == 0) {
    
    hasConc <- ibwQchem %>% 
      filter(!is.na(concentration)) %>%
      distinct(analyte) %>%
      slice(1:1)
    
    ibwQchem <- ibwQchem %>% 
      filter(analyte == hasConc[['analyte']]) %>% 
      arrange(dateTime)
    
    plot(x=as.POSIXct(ibwQchem$dateTime),
         y=ibwQchem$Qls,
         lwd=2, col="blue", xaxt='n', xlab='', yaxt='n', ylab='', type = 'l')
    title(main = chem,
          line = -1,
          cex.main = 0.9)
    
  } else {
    
    ibwQchem <- ibwQchem[ibwQchem$analyte == chem,]
    ibwQchem <- ibwQchem[order(ibwQchem$dateTime),]
    
    templist <- na.approx(ibwQchem$concentration,
                          x=ibwQchem$dateTime,
                          na.rm=FALSE)
    plot(x=as.POSIXct(ibwQchem$dateTime),
         y=ibwQchem$Qls,
         lwd=2, col="blue", xaxt='n', xlab='', yaxt='n', ylab='', type = 'l')
    title(main = chem,
          line = -1,
          cex.main = 0.9)
    par(new=T)
    plot(x=as.POSIXct(ibwQchem$dateTime),
         y=templist,
         type="l", col='green', xaxt='n', yaxt='n', xlab = '', ylab = '', lwd = 3)
    points(x=ibwQchem$dateTime,
           y=ibwQchem$concentration,
           pch=16, col="black", xaxt='n', yaxt='n', xlab='', ylab='')
    
  }
  
}

# e.g., stormChemPlot(15, 'NO3D_LACHAT')


# which analytes are the most common
View(apply(ibw_q_chem[,11:32], 2, function(x) length(which(!is.na(x)))))


# stormFacetPlot is a function to generate (as a PDF) a facet of plots for a
# given storm (as storm mark) for the most common analytes. Inputs include the
# stormid (as storm mark).
stormFacetPlot <- function(stormid) {

  pdf(paste0("./stormwater_chemistry_", stormid, ".pdf"))
  par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,4))
  stormChemPlot(stormid, 'NH4_LACHAT')
  stormChemPlot(stormid, 'NO3D_LACHAT')
  stormChemPlot(stormid, 'CaD_IC')
  stormChemPlot(stormid, 'NaD_IC')
  stormChemPlot(stormid, 'ClD_IC')
  stormChemPlot(stormid, 'PO4D_LACHAT')
  stormChemPlot(stormid, 'NO3T_TOC_TN')
  stormChemPlot(stormid, 'DOC_TOC')
  stormChemPlot(stormid, 'NO2D_LACHAT')
  stormChemPlot(stormid, 'ZnD_ICP')
  stormChemPlot(stormid, 'PO4T_AQ2')
  stormChemPlot(stormid, 'NO3T_AQ2')
  mtext(paste0("stormMark: ", stormid), outer = TRUE)
  dev.off()
  
}


# which storms have chem data?
stormsWithChem <- ibwQchem %>% 
  ilter(!is.na(concentration)) %>% 
  istinct(stormMark)


# generate faceted plots for storms with at least some chem data
for(i in stormsWithChem$stormMark) {
 stormFacetPlot(i)
}