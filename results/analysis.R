library(ggplot2)
library(dplyr)

setwd("~/github/estesReplication/results/")

rawData<-read.csv("processed.csv")

# clean up data...
dataStep3<-subset(rawData,subset=correct==1) # remove errors

data<-subset(dataStep3,subset=response_time<1000) # remove RTs greater than 1000 ms

data <- rename(data, rt = response_time)



# PERFORMANCE MEASURES
# RT
aggRT=aggregate(rt~subject_nr+congruity+targetSide,data=data,FUN="mean") # RT performance data aggregated by subject
RT.aov=aov(rt~congruity*targetSide+Error(as.factor(subject_nr)/(congruity*targetSide)),data=aggRT)
summary(RT.aov)
print(model.tables(RT.aov,"means"),digits=3)

# Init
aggInit=aggregate(init_time~subject_nr+congruity+targetSide,data=data,FUN="mean") # RT performance data aggregated by subject
Init.aov=aov(init_time~congruity*targetSide+Error(as.factor(subject_nr)/(congruity*targetSide)),data=aggInit)
summary(Init.aov)
print(model.tables(Init.aov,"means"),digits=3)

# MT
data$MT <- data$rt-data$init_time
aggMT=aggregate(MT~subject_nr+congruity+targetSide,data=data,FUN="mean") # RT performance data aggregated by subject
MT.aov=aov(MT~congruity*targetSide+Error(as.factor(subject_nr)/(congruity*targetSide)),data=aggMT)
summary(MT.aov)
print(model.tables(MT.aov,"means"),digits=3)

#-----------------
# flankr modeling
#

# tidy up some of the variable names
flankrData <- rawData 
flankrData <- rename(flankrData, congruency = congruity)

# change RT to seconds
flankrData$rt <- as.numeric(flankrData$rt)/1000

#------------------------------------------------------------------------------
### do the flankr modelling

# get only the relevant data columns
data2 <- flankrData %>%
  select(subject_nr, congruency, accuracy, rt)

# do some modelling (this can take AGES)
dstp_fit <- fitDSTP(data2)
ssp_fit <- fitSSP(data2)

# store the model plots
pdf("DSTP_fitExp2.pdf", width = 8, height = 8)
plotFitDSTP(modelFit = dstp_fit, data = data2[complete.cases(data2),])
dev.off()

pdf("SSP_fitExp2.pdf", width = 8, height = 8)
ssp_plot <- plotFitSSP(modelFit = ssp_fit, data = data2[complete.cases(data2),])
dev.off()


