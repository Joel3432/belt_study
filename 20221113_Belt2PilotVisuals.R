#code for Belt Study  2.0 Pilot Data Exploration
#written by: Megan 
#November 2022

#Section outline

#1 - load libraries
#2 - load file
#3 - assign variables
#4 - explore data 

#------------------------1 - load libraries --------------------------

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ARTool)
library(emmeans)
library(multcomp)
library(DescTools)
library(car)
library(rcompanion)

#-----------------------2 - load file -------------------------------
# load data from spreadsheet
belt2pilotdata <- read_excel("20221110_Belt2PilotResults.xlsx", sheet = "ForceData")
belt2EMGdata <- read_excel("20221110_Belt2PilotResults.xlsx", sheet = "EMGData")

#-----------------------3 - assign variables ------------------------

subject <- belt2pilotdata$Subject
timepoint <- belt2pilotdata$Timepoint
QSellipsearea <- belt2pilotdata$QSellipsearea
QSrangeAP <- belt2pilotdata$QSrangeAP
QSrangeML <- belt2pilotdata$QSrangeML
QSvelocity <- belt2pilotdata$QSvelocity
QStotalex <- belt2pilotdata$QSTotalEx
QSellipseasym <- belt2pilotdata$QSEllipseAsym
SLLellipsearea <- belt2pilotdata$SLLellipsearea
SLLrangeAP <- belt2pilotdata$SLLrangeAP
SLLrangeML <- belt2pilotdata$SLLrangeML
SLLvelocity <- belt2pilotdata$SLLvelocity
SLLtotalex <- belt2pilotdata$SLLTotalEx
SLRellipsearea <- belt2pilotdata$SLRellipsearea
SLRrangeAP <- belt2pilotdata$SLRrangeAP
SLRrangeML <- belt2pilotdata$SLRrangeML
SLRvelocity <- belt2pilotdata$SLRvelocity
SLRtotalex <- belt2pilotdata$SLRTotalEx
squatmeanforce <- belt2pilotdata$SquatConMeanForce
squatasym <- belt2pilotdata$SquatConAsym
CMJheight <- belt2pilotdata$CMJheight
CMJRFD <- belt2pilotdata$CMJConRFD
CMJRFDasym <- belt2pilotdata$CMJConRFDAsym
CMJpower <- belt2pilotdata$CMJPeakPower
IMTPforce <- belt2pilotdata$IMTPPeakForce
IMTPpeakasym <- belt2pilotdata$IMTPPeakFAsym
IMTPFFD150 <- belt2pilotdata$IMPTRFD150
IMTPRFDasym <- belt2pilotdata$IMTPAsym150
EMGsubject <- belt2EMGdata$Subject
EMGtask <- belt2EMGdata$Task
EMGtimpoint <- belt2EMGdata$Timepoint
RAB <- belt2EMGdata$RAB
LAB <- belt2EMGdata$LAB
RMF <- belt2EMGdata$RMF
LMF <- belt2EMGdata$LMF
RABrel <- belt2EMGdata$RABrel
LABrel <- belt2EMGdata$LABrel
RMFrel <- belt2EMGdata$RMFrel
LMFrel <- belt2EMGdata$LMFrel


subject<-as.factor(belt2pilotdata$Subject) 
timepoint<-as.numeric(belt2pilotdata$Timepoint) 
EMGsubject<-as.factor(belt2EMGdata$Subject) 
EMGtimepoint<-as.factor(belt2EMGdata$Timepoint) 

EMGtask <- factor(EMGtask,levels = c(1,2,3,4,5,6,7),labels = c("Walking", "Quiet Standing", "Single Leg Left","Single Leg Right", "Squatting", "CMJ", "IMTP"))

# --------------------- visualize the data ---------------------------
#line plot of variables across day 

# Plot QSellipsearea
ggplot(belt2pilotdata, aes(x=timepoint, y=QSellipsearea, group = subject, color = subject)) +
    geom_line()

# plot QSrangeAP 
ggplot(belt2pilotdata, aes(x=timepoint, y=QSrangeAP, group = subject, color = subject)) +
  geom_line()

#plot QSrangeML
ggplot(belt2pilotdata, aes(x=timepoint, y=QSrangeML, group = subject, color = subject)) +
  geom_line()

#plot QS velocity 
ggplot(belt2pilotdata, aes(x=timepoint, y=QSvelocity, group = subject, color = subject)) +
  geom_line()

#plot QS total excursion 
ggplot(belt2pilotdata, aes(x=timepoint, y=QStotalex, group = subject, color = subject)) +
  geom_line()

#plot QS asymmetry 
ggplot(belt2pilotdata, aes(x=timepoint, y=QSellipseasym, group = subject, color = subject)) +
  geom_line()

#plot SLL ellipseare
ggplot(belt2pilotdata, aes(x=timepoint, y=SLLellipsearea, group = subject, color = subject)) +
  geom_line()

#plot SLR ellipseare
ggplot(belt2pilotdata, aes(x=timepoint, y=SLRellipsearea, group = subject, color = subject)) +
  geom_line()

#plot CMJ height 

ggplot(belt2pilotdata, aes(x=timepoint, y=CMJheight, group = subject, color = subject)) +
  geom_line()

#plot CMJ RFD asymmetry 

ggplot(belt2pilotdata, aes(x=timepoint, y=CMJRFDasym, group = subject, color = subject)) +
  geom_line()

#plot CMJ peak power 
ggplot(belt2pilotdata, aes(x=timepoint, y=CMJpower, group = subject, color = subject)) +
  geom_line()

#plot IMTP peak force 
ggplot(belt2pilotdata, aes(x=timepoint, y=IMTPforce, group = subject, color = subject)) +
  geom_line()

#plot IMTP 150 RFD

ggplot(belt2pilotdata, aes(x=timepoint, y=IMTPFFD150, group = subject, color = subject)) +
  geom_line()

#plot IMTP 150 RFD asym
ggplot(belt2pilotdata, aes(x=timepoint, y=IMTPRFDasym, group = subject, color = subject)) +
  geom_line()

#plot IMTP peak force asym 
ggplot(belt2pilotdata, aes(x=timepoint, y=IMTPpeakasym, group = subject, color = subject)) +
  geom_line()


#descriptives for EMG data 
aggregate(belt2EMGdata$LABrel, list(belt2EMGdata$Timepoint,belt2EMGdata$Task), FUN=mean)
aggregate(belt2EMGdata$LABrel, list(belt2EMGdata$Timepoint, belt2EMGdata$Task), FUN=sd)

aggregate(belt2EMGdata$RABrel, list(belt2EMGdata$Timepoint,belt2EMGdata$Task), FUN=mean)
aggregate(belt2EMGdata$RABrel, list(belt2EMGdata$Timepoint, belt2EMGdata$Task), FUN=sd)

aggregate(belt2EMGdata$LMFrel, list(belt2EMGdata$Timepoint,belt2EMGdata$Task), FUN=mean)
aggregate(belt2EMGdata$LMFrel, list(belt2EMGdata$Timepoint, belt2EMGdata$Task), FUN=sd)

aggregate(belt2EMGdata$RMFrel, list(belt2EMGdata$Timepoint,belt2EMGdata$Task), FUN=mean)
aggregate(belt2EMGdata$RMFrel, list(belt2EMGdata$Timepoint, belt2EMGdata$Task), FUN=sd)



ggplot(data=belt2EMGdata, aes(fill=EMGtimepoint, y=LMFrel, x=EMGtask)) + 
  geom_bar(position="dodge", stat="identity", color = "black")
ggplot(data=belt2EMGdata, aes(fill=EMGtimepoint, y=RMFrel, x=EMGtask)) + 
  geom_bar(position="dodge", stat="identity", color = "black")
ggplot(data=belt2EMGdata, aes(fill=EMGtimepoint, y=LABrel, x=EMGtask)) + 
  geom_bar(position="dodge", stat="identity")
ggplot(data=belt2EMGdata, aes(fill=EMGtimepoint, y=RABrel, x=EMGtask)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(data=belt2EMGdata, aes(x=EMGtask, y=RMFrel, fill=EMGtimepoint)) +
  geom_boxplot() + geom_jitter(color="black", size=0.4, alpha=0.9) 

ggplot(data=belt2EMGdata, aes(x=EMGtask, y=LMFrel, fill=EMGtimepoint)) +
  geom_boxplot() + geom_jitter(color="black", size=0.4, alpha=0.9) 

ggplot(data=belt2EMGdata, aes(x=EMGtask, y=LABrel, fill=EMGtimepoint)) +
  geom_boxplot() + geom_jitter(color="black", size=0.4, alpha=0.9) 

ggplot(data=belt2EMGdata, aes(x=EMGtask, y=RABrel, fill=EMGtimepoint)) +
  geom_boxplot() + geom_jitter(color="black", size=0.4, alpha=0.9) 



     