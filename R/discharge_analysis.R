#basic intial analyses of storms 
library(plyr)
library(ggplot2)
library(reshape2)
library(plotly)

setwd("C:/Users/Katie Kemmitt/Dropbox (ASU)/SNAZ meeting materials/indianBendWash/data")
wide<-read.csv("ibw_Q_chemistry.csv")
long<-read.csv("ibwQchem.csv")

# start with only storms that have been vetted 9, 10, 11, 14, 15, 16, 17, 29, 32, 33, 
# 34, 37, 39, 42, 22, 67, 74
  data <- long %>% filter(stormMark == c("9", "10", "11", "14", "15", "16", "17", "29", "32", "33",
                               "34", "37", "39", "42", "22", "67", "74"))

#summarize data by storm and analyte
  Qchem<- ddply(data, c("stormMark", "analyte"), summarize, 
              maxQ = max(Qls),
              totalQ = max(cumQ), 
              maxC = max(concentration, na.rm = T))
  
#plot grid with free y axis 
  ggplot(Qchem, aes(x = maxQ, y = maxC))+
    geom_point()+
    geom_smooth(method = "lm", se = FALSE)+
    ylab("Max Concentration")+
    xlab("Max Discharge (L/s)")+
    facet_wrap( ~ analyte, scales = "free_y")

  ggplot(Qchem, aes(x = totalQ, y = maxC))+
    geom_point()+
    geom_smooth(method = "lm", se = FALSE)+
    ylab("Max Concentration")+
    xlab("Total Discharge (liters)")+
    facet_wrap( ~ analyte, scales = "free_y")
