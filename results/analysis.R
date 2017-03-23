library(ggplot2)
library(dplyr)

setwd("~/github/estesReplication/results/")

rawData<-read.csv("processed.csv")

# clean up data...
dataStep3<-subset(rawData,subset=trialType=="test") # remove errors
dataFull<-subset(dataStep3,subset=response_time<1000) # remove RTs greater than 1000 ms and fillers
dataFull$typicalLocation <- factor(data$typicalLocation,levels=c("bottom","top")) # removes "filler" from level set of factor
dataFull <- rename(dataFull, rt = response_time)

# code condition as typical or atypical
condition <- numeric(length(dataFull$rt))
for (i in 1:length(condition)){
  if (dataFull$location[i]==dataFull$typicalLocation[i]){
    condition[i] <- "typical"
  }
  else {
    condition[i] <- "atypical"
  }
}

dataFull$condition <- condition

# PERFORMANCE MEASURES
# RTs
# remove errors
data <- subset(dataFull,subset=correct==1)
# F_1 -- across participants
aggRT1=aggregate(rt~subject_nr+condition,data=data,FUN="mean") # RT performance data aggregated by subject
RT1.aov=aov(rt~condition+Error(as.factor(subject_nr)/condition),data=aggRT1)
summary(RT1.aov)
print(model.tables(RT1.aov,"means"),digits=3)

# F_2 -- across items (using "cueWord")
aggRT2=aggregate(rt~cueWord+condition,data=data,FUN="mean") # RT performance data aggregated by subject
RT2.aov=aov(rt~condition,data=aggRT2)
summary(RT2.aov)
print(model.tables(RT2.aov,"means"),digits=3)


# error rates
aggErr <- aggregate(correct~subject_nr+condition,data=dataFull,FUN="sum")
err1.aov <- aov(correct~condition+Error(as.factor(subject_nr)/condition),data=aggErr)
summary(err1.aov)
print(model.tables(err1.aov,"means"),digits=3)

aggErr2 <- aggregate(correct~cueWord+condition,data=dataFull,FUN="sum")
err2.aov <- aov(correct~condition,data=aggErr)
summary(err2.aov)
print(model.tables(err2.aov,"means"),digits=3)
