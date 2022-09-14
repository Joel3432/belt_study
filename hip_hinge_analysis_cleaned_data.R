#Belt Study Hinge Paper
#written by:  James Kearney & Dr. Joel Martin
#April-July 2022

#Section outline

#1 - load libraries
#2 - load file
#3 - assign variables
#4 - Explore data visually
#5 - Winsorize Extreme Values
#6 - Explore data after winsorizing
#7 - Check normality 
#8 - Transform Data & Recheck normality
#9 - Test for significant differences by gender on age, height, mass, BMI
#10 - MANOVA for 1) No-load sEMG RMS, 2)Bar lift peak normalized EMG 3) 95lb Bar lift peak normalized EMG


#Factors
#Effect of Condition on muscles
#Demographics on Condition
#Condition on Muscle stratified by comfort

#------------------------1 - load libraries --------------------------
library(git2r)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(tidyverse)
library(readxl)
library(data.table)
library(ggpubr)
library(readxl)
library(rstatix)
library(car)
library(fmsb)
library(Hmisc)
library(gridExtra)
library(corrplot)
library(MANOVA.RM)
library(psych)
library(doBy)
library(mvnormtest)
library(rrr)
library(na.tools)
library(robustHD)
library(jtools)
library(npmv)
library(lsr)
library(effsize)
library(MASS)
library(effectsize)
library(ARTool)
library(DescTools)

#-----------------------2 - load file -------------------------------
# load data from spreadsheet
hdata <- read_excel("20220914_cleaned_data.xlsx",sheet = "cleaned_data_normalized")
my_data<-na.omit(hdata)


#-----------------------3 - assign variables ------------------------
## CHANGE SUBJECT AND CONDITION TO FACTORS
hdata$Subject<-as.factor(hdata$Subject) 
hdata$Condition<-as.factor(hdata$Condition_coded) 
h_data<-na.omit(hdata)
#CHECK THE DATA SET STRUCTURE
str(hdata)

#LABEL IV
hdata$Condition <- factor(hdata$Condition,levels = c(1,2,3,4),labels = c("Control", "Leather belt","Nylon Belt","Vest")) 

#BMI to Categories (3 = Yellow = 25-29.9, 2 = Green = 18 - 24.9, 1 = Blue < 17.9)
hdata$BMI<-as.factor(hdata$BMI)
hdata$BMI <- factor(hdata$BMI,levels = c(2,3),labels = c("Green", "Yellow"))

#CHECK THE DATA SET STRUCTURE
str(hdata)


#______________________________________________________________________________________________________
#------------------------4- Explore data visually---------------------------------------------------

#Our variables of interest for the paper are:
#Task unloaded hinging (NL) - RMS of each muscle
#Task bar  (Br) - peak emg of each muscle
#loaded bar lifting (max) - peak emg of each muscle
#vGRF for bodyweight hinging

#-------------------------------------NL RMS-------------------------------------------
ggplot(hdata, aes(x=Condition, y=NL_RMS_LVL)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL RMS LVL")

ggplot(hdata, aes(x=Condition, y=NL_RMS_RVL)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL RMS RVL")

ggplot(hdata, aes(x=Condition, y=NL_RMS_LBF)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL RMS LBF")

ggplot(hdata, aes(x=Condition, y=NL_RMS_RBF)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL RMS RBF")

ggplot(hdata, aes(x=Condition, y=NL_RMS_LRA)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL RMS LRA")

ggplot(hdata, aes(x=Condition, y=NL_RMS_RRA)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL RMS RRA")

ggplot(hdata, aes(x=Condition, y=NL_RMS_LMT)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL RMS LMT")

ggplot(hdata, aes(x=Condition, y=NL_RMS_RMT)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL RMS RMT")

#-------------------------------------NL Mean-------------------------------------------
ggplot(hdata, aes(x=Condition, y=NL_Mean_LVL)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Mean LVL")

ggplot(hdata, aes(x=Condition, y=NL_Mean_RVL)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Mean RVL")

ggplot(hdata, aes(x=Condition, y=NL_Mean_LBF)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Mean LBF")

ggplot(hdata, aes(x=Condition, y=NL_Mean_RBF)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Mean RBF")

ggplot(hdata, aes(x=Condition, y=NL_Mean_LRA)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Mean LRA")

ggplot(hdata, aes(x=Condition, y=NL_Mean_RRA)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Mean RRA")

ggplot(hdata, aes(x=Condition, y=NL_Mean_LMT)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Mean LMT")

ggplot(hdata, aes(x=Condition, y=NL_Mean_RMT)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Mean RMT")

#-------------------------------------NL Peak-------------------------------------------
ggplot(hdata, aes(x=Condition, y=NL_Peak_LVL)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Peak LVL")

ggplot(hdata, aes(x=Condition, y=NL_Peak_RVL)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Peak RVL")

ggplot(hdata, aes(x=Condition, y=NL_Peak_LBF)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Peak LBF")

ggplot(hdata, aes(x=Condition, y=NL_Peak_RBF)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Peak RBF")

ggplot(hdata, aes(x=Condition, y=NL_Peak_LRA)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Peak LRA")

ggplot(hdata, aes(x=Condition, y=NL_Peak_RRA)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Peak RRA")

ggplot(hdata, aes(x=Condition, y=NL_Peak_LMT)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Peak LMT")

ggplot(hdata, aes(x=Condition, y=NL_Peak_RMT)) + 
  geom_boxplot()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                              outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("NL Peak RMT")


#------------------------------Bar Peak-------------------------------------
ggplot(hdata, aes(x=Condition, y=Br_Peak_LVL)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Bar Peak EMG LVL")

ggplot(hdata, aes(x=Condition, y=Br_Peak_RVL)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Bar Peak EMG RVL")

ggplot(hdata, aes(x=Condition, y=Br_Peak_LBF)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Bar Peak EMG LBF")

ggplot(hdata, aes(x=Condition, y=Br_Peak_RBF)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Bar Peak EMG RBF")

ggplot(hdata, aes(x=Condition, y=Br_Peak_LRA)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Bar Peak EMG LRA")

ggplot(hdata, aes(x=Condition, y=Br_Peak_RRA)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Bar Peak EMG RRA")

ggplot(hdata, aes(x=Condition, y=Br_Peak_LMT)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Bar Peak EMG LMT")

ggplot(hdata, aes(x=Condition, y=Br_Peak_RMT)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Bar Peak EMG RMT")

#------------------------------Loaded Bar Peak-------------------------------------
ggplot(hdata, aes(x=Condition, y=max_Peak_LVL)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Loaded Bar Peak EMG LVL")

ggplot(hdata, aes(x=Condition, y=max_Peak_RVL)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Loaded Bar Peak EMG RVL")

ggplot(hdata, aes(x=Condition, y=max_Peak_LBF)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Loaded Bar Peak EMG LBF")

ggplot(hdata, aes(x=Condition, y=max_Peak_RBF)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Loaded Bar Peak EMG RBF")

ggplot(hdata, aes(x=Condition, y=max_Peak_LRA)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Loaded Bar Peak EMG LRA")

ggplot(hdata, aes(x=Condition, y=max_Peak_RRA)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Loaded Bar Peak EMG RRA")

ggplot(hdata, aes(x=Condition, y=max_Peak_LMT)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Loaded Bar Peak EMG LMT")

ggplot(hdata, aes(x=Condition, y=max_Peak_RMT)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("Loaded Bar Peak EMG RMT")


#-------------------------------vGRF---------------------------------------

#Peak-----------------------------------------------------------
ggplot(hdata, aes(x=Condition, y=PeakForce_L_N)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("PeakForce_L_N")

ggplot(hdata, aes(x=Condition, y=PeakForce_R_N)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("PeakForce_R_N")

ggplot(hdata, aes(x=Condition, y=total_PeakForce_N)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("total_PeakForce_N")

ggplot(hdata, aes(x=Condition, y=peakForceAsymm_percent)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("peakForceAsymm_percent")



#-----%-of-BW-------------------------------------------------------
ggplot(hdata, aes(x=Condition, y=peakForceNorm_L_BW)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("peakForceNorm_L_BW")

ggplot(hdata, aes(x=Condition, y=peakForceNorm_R_BW)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("peakForceNorm_R_BW")

ggplot(hdata, aes(x=Condition, y=total_PeakForce_BW)) + 
  geom_boxplot()+ geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=4)+geom_jitter(shape=16, position=position_jitter(0.2))+ggtitle("total_PeakForce_BW")

#------------------------#7 - Check normality ----------------------------------
#---------------------------------- Q-Q Plot
#RMS
qqnorm(hdata$NL_RMS_LVL)
qqline(hdata$NL_RMS_LVL)
qqnorm(hdata$NL_RMS_RVL)
qqline(hdata$NL_RMS_RVL)
qqnorm(hdata$NL_RMS_LBF)
qqline(hdata$NL_RMS_LBF)
qqnorm(hdata$NL_RMS_RBF)
qqline(hdata$NL_RMS_RBF)
qqnorm(hdata$NL_RMS_LRA)
qqline(hdata$NL_RMS_LRA)
qqnorm(hdata$NL_RMS_RRA)
qqline(hdata$NL_RMS_RRA)
qqnorm(hdata$NL_RMS_LMT)
qqline(hdata$NL_RMS_LMT)
qqnorm(hdata$NL_RMS_RMT)
qqline(hdata$NL_RMS_RMT)

#Normality-----------------------------------
d<-density(hdata$Age)
plot(d)
d<-density(hdata$Height)
plot(d)
d<-density(hdata$Mass)
plot(d)
d<-density(hdata$BMI_V)
plot(d)

shapiro.test(hdata$Age)
shapiro.test(hdata$Sex)
shapiro.test(hdata$Height)
shapiro.test(hdata$Mass)
shapiro.test(hdata$BMI_V)
shapiro.test(hdata$Comfort)
shapiro.test(hdata$Comfort_Rank)
shapiro.test(hdata$Restrictive)
shapiro.test(hdata$Rstrictive_Rank)

#----------RMS NL----------------------
shapiro.test(hdata$NL_RMS_LVL)
shapiro.test(hdata$NL_RMS_RVL)
shapiro.test(hdata$NL_RMS_LBF)
shapiro.test(hdata$NL_RMS_RBF)
shapiro.test(hdata$NL_RMS_LRA) 
shapiro.test(hdata$NL_RMS_RRA)
shapiro.test(hdata$NL_RMS_LMT)
shapiro.test(hdata$NL_RMS_RMT)

#---------Mean EMG NL------------
shapiro.test(hdata$NL_Mean_LVL)
shapiro.test(hdata$NL_Mean_RVL)
shapiro.test(hdata$NL_Mean_LBF)
shapiro.test(hdata$NL_Mean_RBF)
shapiro.test(hdata$NL_Mean_LRA) 
shapiro.test(hdata$NL_Mean_RRA)  
shapiro.test(hdata$NL_Mean_LMT)
shapiro.test(hdata$NL_Mean_RMT)

#---------Peak EMG NL----------------------
shapiro.test(hdata$NL_Peak_LVL)
shapiro.test(hdata$NL_Peak_RVL)
shapiro.test(hdata$NL_Peak_LBF)
shapiro.test(hdata$NL_Peak_RBF)
shapiro.test(hdata$NL_Peak_LRA) 
shapiro.test(hdata$NL_Peak_RRA) 
shapiro.test(hdata$NL_Peak_LMT)
shapiro.test(hdata$NL_Peak_RMT)

#---------Peak EMG -Bar Lift---------------------
shapiro.test(hdata$Br_Peak_LVL)
shapiro.test(hdata$Br_Peak_RVL)
shapiro.test(hdata$Br_Peak_LBF)
shapiro.test(hdata$Br_Peak_RBF)
shapiro.test(hdata$Br_Peak_LRA) 
shapiro.test(hdata$Br_Peak_RRA)
shapiro.test(hdata$Br_Peak_LMT)
shapiro.test(hdata$Br_Peak_RMT)

#---------Peak EMG -Max Lift---------------------
shapiro.test(hdata$max_Peak_LVL)
shapiro.test(hdata$max_Peak_RVL)
shapiro.test(hdata$max_Peak_LBF)
shapiro.test(hdata$max_Peak_RBF)
shapiro.test(hdata$max_Peak_LRA) 
shapiro.test(hdata$max_Peak_RRA) 
shapiro.test(hdata$max_Peak_LMT)
shapiro.test(hdata$max_Peak_RMT)

#-------------------------------------vGRF
shapiro.test(hdata$PeakForce_L_N)
shapiro.test(hdata$PeakForce_R_N)
shapiro.test(hdata$total_PeakForce_N)
shapiro.test(hdata$peakForceAsymm_percent)
shapiro.test(hdata$peakForceNorm_L_BW)
shapiro.test(hdata$peakForceNorm_R_BW)
shapiro.test(hdata$total_PeakForce_BW)

shapiro.test(hdata2$meanForce_L_N)
shapiro.test(hdata2$meanForce_R_N)
shapiro.test(hdata2$totalMeanForce_N)
shapiro.test(hdata2$meanForceAsymm_percent)
shapiro.test(hdata2$meanForceNorm_L)
shapiro.test(hdata2$meanForceNorm_R)
shapiro.test(hdata2$totalmeanforceNorm)

#--------------------------Multivariate Normality
hRMS <- t(hdata[8:15])
mshapiro.test(hRMS)

hMean <- t(hdata[16:23])
mshapiro.test(hMean)

hPeak <- t(hdata[24:31])
mshapiro.test(hPeak)

hBar <- t(hdata[32:39])
mshapiro.test(hBar)

hMax <- t(hdata[40:47])
mshapiro.test(hMax)


#------8 Try Transformation of data to see if normalizes-------
NL_RMS_LVL_log <- log(hdata$NL_RMS_LVL)
shapiro.test(NL_RMS_LVL_log)
NL_RMS_RVL_log <- log(hdata$NL_RMS_RVL)
shapiro.test(NL_RMS_RVL_log)
NL_RMS_LBF_log <- log(hdata$NL_RMS_LBF)
shapiro.test(NL_RMS_LBF_log)
NL_RMS_RBF_log <- log(hdata$NL_RMS_RBF)
shapiro.test(NL_RMS_RBF_log)
#Not working for these

Br_Peak_LVL_log <- log(hdata$Br_Peak_LVL)
shapiro.test(Br_Peak_LVL_log)
Br_Peak_RVL_log <- log(hdata$Br_Peak_RVL)
shapiro.test(Br_Peak_RVL_log)
Br_Peak_LBF_log <- log(hdata$Br_Peak_LBF)
shapiro.test(Br_Peak_LBF_log)
Br_Peak_RBF_log <- log(hdata$Br_Peak_RBF)
shapiro.test(Br_Peak_RBF_log)
#Not working for these

max_Peak_LVL_log <- log(hdata$max_Peak_LVL)
shapiro.test(max_Peak_LVL_log)
max_Peak_RVL_log <- log(hdata$max_Peak_RVL)
shapiro.test(max_Peak_RVL_log)
max_Peak_LBF_log <- log(hdata$max_Peak_LBF)
shapiro.test(max_Peak_LBF_log)
max_Peak_RBF_log <- log(hdata$max_Peak_RBF)
shapiro.test(max_Peak_RBF_log)
#Not working for these

PeakForce_R_N_log <- log(hdata$PeakForce_R_N)
shapiro.test(PeakForce_R_N_log)
peakForceAsymm_percent_log <- log(hdata$peakForceAsymm_percent)
shapiro.test(peakForceAsymm_percent_log)
peakForceNorm_L_BW_log <- log(hdata$peakForceNorm_L_BW)
shapiro.test(peakForceNorm_L_BW_log)
#Not working for these


#-------9 Descriptive Statistics------------------------------
#This will compute mean and standard deviation for each variable by condition factor

summaryBy(Comfort~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Comfort_Rank ~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Restrictive ~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Rstrictive_Rank ~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )


#----------------RMS-BW Hinge---------------------------------------------
summaryBy(NL_RMS_LVL~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_RVL~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_LBF~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_RBF~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_LRA~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_RRA~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_LMT~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_RMT~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )



#---------------------------Bar-Peak---------------------------------
summaryBy(Br_Peak_LVL~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_RVL~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_LBF~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_RBF~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_LRA~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_RRA~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_LMT~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_RMT~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )



#------------------------Max-Peak---------------------------------
summaryBy(max_Peak_LVL~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_RVL~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_LBF~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_RBF~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_LRA~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_RRA~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_LMT~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_RMT~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )


#----------------vGRF--------------------------------------
summaryBy(PeakForce_L_N ~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(PeakForce_R_N ~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(total_PeakForce_N ~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(peakForceAsymm_percent ~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(peakForceNorm_L_BW ~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(peakForceNorm_R_BW ~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(total_PeakForce_BW ~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(meanForce_L_N~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(meanForce_R_N~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(totalMeanForce_N~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(meanForceAsymm_percent~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(meanForceNorm_L~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(meanForceNorm_R~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(totalmeanforceNorm~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

#------------------------9 - Test for significant differences by gender on age, height, mass, BMI----------

#make data set just for demographic from condition 1
demo_df <-filter(hdata, Condition=="Control")
describe(demo_df$Age)
describeBy(Age + Height + Mass + BMI_V~Sex, data=demo_df)

#Run 1-way anova to see if differences by sex
anova_age_sex <- aov(Age ~Sex,data=demo_df)
summary(anova_age_sex)
cohens_d(Age ~Sex,data=demo_df)

anova_age_height <- aov(Height ~Sex,data=demo_df)
summary(anova_age_height)
cohens_d(Height ~Sex,data=demo_df)

anova_age_mass <- aov(Mass ~Sex,data=demo_df)
summary(anova_age_mass)
cohens_d(Mass ~Sex,data=demo_df)

anova_age_bmi <- aov(BMI_V ~Sex,data=demo_df)
summary(anova_age_bmi)
cohens_d(BMI_V ~Sex,data=demo_df)


####___________________________________________________________________________________
#------------------------10 - Manova --------------------------
#Non-parametric ARTool analysis 
#https://depts.washington.edu/acelab/proj/art/

#art transforms the data
#summary to check that art was done properly ( should be 0's)
#run as ANOVA
#to add ES as partial eta-squared: https://cran.r-project.org/web/packages/ARTool/vignettes/art-effect-size.html

#Non-loaded hip hinging
NL_artLVL = art(NL_RMS_LVL ~ Condition + (1|Subject), data=hdata)
summary(NL_artLVL)
anova(NL_artLVL)

## This computes the effect size -> partial eta squared
m.linear = lm(NL_RMS_LVL ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

#Post-hoc for sig differences
art.con(NL_artLVL, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))


NL_artRVL = art(NL_RMS_RVL ~ Condition + (1|Subject), data=hdata)
summary(NL_artRVL)
anova(NL_artRVL)
m.linear = lm(NL_RMS_RVL ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

NL_artLBF = art(NL_RMS_LBF ~ Condition + (1|Subject), data=hdata)
summary(NL_artLBF)
anova(NL_artLBF)
m.linear = lm(NL_RMS_LBF ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

art.con(NL_artLBF, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

NL_artRBF = art(NL_RMS_RBF ~ Condition + (1|Subject), data=hdata)
summary(NL_artRBF)
anova(NL_artRBF)
m.linear = lm(NL_RMS_RBF ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
#Post-hoc for sig differences
art.con(NL_artRBF, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

NL_artLRA = art(NL_RMS_LRA ~ Condition + (1|Subject), data=hdata)
summary(NL_artLRA)
anova(NL_artLRA)
m.linear = lm(NL_RMS_LRA ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
#Post-hoc for sig differences
art.con(NL_artLRA, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

NL_artRRA = art(NL_RMS_RRA ~ Condition + (1|Subject), data=hdata)
summary(NL_artRRA)
anova(NL_artRRA)
m.linear = lm(NL_RMS_RRA ~ Condition, data=hdata)
EtaSq(m.linear, type=3)


NL_artLMT = art(NL_RMS_LMT ~ Condition + (1|Subject), data=hdata)
summary(NL_artLMT)
anova(NL_artLMT)
m.linear = lm(NL_RMS_LMT ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
#Post-hoc for sig differences
art.con(NL_artLMT, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

NL_artRMT = art(NL_RMS_RMT ~ Condition + (1|Subject), data=hdata)
summary(NL_artRMT)
anova(NL_artRMT)
m.linear = lm(NL_RMS_RMT ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
#Post-hoc for sig differences
art.con(NL_artRMT, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#mean EMG
NL_artLVL_mean = art(NL_Mean_LVL ~ Condition + (1|Subject), data=hdata)
summary(NL_artLVL_mean)
anova(NL_artLVL_mean)
m.linear = lm(NL_Mean_LVL ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

art.con(NL_artLVL_mean, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))


NL_artRVL_mean = art(NL_Mean_RVL ~ Condition + (1|Subject), data=hdata)
summary(NL_artRVL_mean)
anova(NL_artRVL_mean)
m.linear = lm(NL_Mean_RVL ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(NL_artRVL_mean, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

NL_artLBF_mean = art(NL_Mean_LBF ~ Condition + (1|Subject), data=hdata)
summary(NL_artLBF_mean)
anova(NL_artLBF_mean)
m.linear = lm(NL_Mean_LBF ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(NL_artLBF_mean, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

NL_artRBF_mean = art(NL_Mean_RBF ~ Condition + (1|Subject), data=hdata)
summary(NL_artRBF_mean)
anova(NL_artRBF_mean)
m.linear = lm(NL_Mean_RBF ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(NL_artRBF_mean, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

NL_artLRA_mean = art(NL_Mean_LRA ~ Condition + (1|Subject), data=hdata)
summary(NL_artLRA_mean)
anova(NL_artLRA_mean)
m.linear = lm(NL_Mean_LRA ~ Condition, data=hdata)
EtaSq(m.linear, type=3)


NL_artRRA_mean = art(NL_Mean_RRA ~ Condition + (1|Subject), data=hdata)
summary(NL_artRRA_mean)
anova(NL_artRRA_mean)
m.linear = lm(NL_Mean_RRA ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

NL_artLMT_mean = art(NL_Mean_LMT ~ Condition + (1|Subject), data=hdata)
summary(NL_artLMT_mean)
anova(NL_artLMT_mean)
m.linear = lm(NL_Mean_LMT ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(NL_artLMT_mean, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

NL_artRMT_mean = art(NL_Mean_RMT ~ Condition + (1|Subject), data=hdata)
summary(NL_artRMT_mean)
anova(NL_artRMT_mean)
m.linear = lm(NL_Mean_RMT ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(NL_artRMT_mean, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#peak emg
NL_artLVL_Peak = art(NL_Peak_LVL ~ Condition + (1|Subject), data=hdata)
summary(NL_artLVL_Peak)
anova(NL_artLVL_Peak)
m.linear = lm(NL_Peak_LVL ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

NL_artRVL_Peak = art(NL_Peak_RVL ~ Condition + (1|Subject), data=hdata)
summary(NL_artRVL_Peak)
anova(NL_artRVL_Peak)
m.linear = lm(NL_Peak_RVL ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

NL_artLBF_Peak = art(NL_Peak_LBF ~ Condition + (1|Subject), data=hdata)
summary(NL_artLBF_Peak)
anova(NL_artLBF_Peak)
m.linear = lm(NL_Peak_LBF ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

NL_artRBF_Peak = art(NL_Peak_RBF ~ Condition + (1|Subject), data=hdata)
summary(NL_artRBF_Peak)
anova(NL_artRBF_Peak)
m.linear = lm(NL_Peak_RBF ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

NL_artLRA_Peak = art(NL_Peak_LRA ~ Condition + (1|Subject), data=hdata)
summary(NL_artLRA_Peak)
anova(NL_artLRA_Peak)
m.linear = lm(NL_Peak_LRA ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

NL_artRRA_Peak = art(NL_Peak_RRA ~ Condition + (1|Subject), data=hdata)
summary(NL_artRRA_Peak)
anova(NL_artRRA_Peak)
m.linear = lm(NL_Peak_RRA ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

NL_artLMT_Peak = art(NL_Peak_LMT ~ Condition + (1|Subject), data=hdata)
summary(NL_artLMT_Peak)
anova(NL_artLMT_Peak)
m.linear = lm(NL_Peak_LMT ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(NL_artLMT_Peak, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

NL_artRMT_Peak = art(NL_Peak_RMT ~ Condition + (1|Subject), data=hdata)
summary(NL_artRMT_Peak)
anova(NL_artRMT_Peak)
m.linear = lm(NL_Peak_RMT ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(NL_artRMT_Peak, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))


#Lifting Bar only
Br_artLVL = art(Br_Peak_LVL ~ Condition + (1|Subject), data=hdata)
summary(Br_artLVL)
anova(Br_artLVL)
m.linear = lm(Br_Peak_LVL ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(Br_artLVL, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

Br_artRVL = art(Br_Peak_RVL ~ Condition + (1|Subject), data=hdata)
summary(Br_artRVL)
anova(Br_artRVL)
m.linear = lm(Br_Peak_RVL ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(Br_artRVL, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

Br_artLBF = art(Br_Peak_LBF ~ Condition + (1|Subject), data=hdata)
summary(Br_artLBF)
anova(Br_artLBF)
m.linear = lm(Br_Peak_LBF ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

Br_artRBF = art(Br_Peak_RBF ~ Condition + (1|Subject), data=hdata)
summary(Br_artRBF)
anova(Br_artRBF)
m.linear = lm(Br_Peak_RBF ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

Br_artLRA = art(Br_Peak_LRA ~ Condition + (1|Subject), data=hdata)
summary(Br_artLRA)
anova(Br_artLRA)
m.linear = lm(Br_Peak_LRA ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

Br_artRRA = art(Br_Peak_RRA ~ Condition + (1|Subject), data=hdata)
summary(Br_artRRA)
anova(Br_artRRA)
m.linear = lm(Br_Peak_RRA ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

Br_artLMT = art(Br_Peak_LMT ~ Condition + (1|Subject), data=hdata)
summary(Br_artLMT)
anova(Br_artLMT)
m.linear = lm(Br_Peak_LMT ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

Br_artRMT = art(Br_Peak_RMT ~ Condition + (1|Subject), data=hdata)
summary(Br_artRMT)
anova(Br_artRMT)
m.linear = lm(Br_Peak_RMT ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

#95 lb bar
max_artLVL = art(max_Peak_LVL ~ Condition + (1|Subject), data=hdata)
summary(max_artLVL)
anova(max_artLVL)
m.linear = lm(max_Peak_LVL ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

art.con(max_artLVL, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

max_artRVL = art(max_Peak_RVL ~ Condition + (1|Subject), data=hdata)
summary(max_artRVL)
anova(max_artRVL)
m.linear = lm(max_Peak_RVL ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(max_artRVL, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

max_artLBF = art(max_Peak_LBF ~ Condition + (1|Subject), data=hdata)
summary(max_artLBF)
anova(max_artLBF)
m.linear = lm(max_Peak_LBF ~ Condition, data=hdata)
EtaSq(m.linear, type=3)


max_artRBF = art(max_Peak_RBF ~ Condition + (1|Subject), data=hdata)
summary(max_artRBF)
anova(max_artRBF)
m.linear = lm(max_Peak_RBF ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

max_artLRA = art(max_Peak_LRA ~ Condition + (1|Subject), data=hdata)
summary(max_artLRA)
anova(max_artLRA)
m.linear = lm(max_Peak_LRA ~ Condition, data=hdata)
EtaSq(m.linear, type=3)


max_artRRA = art(max_Peak_RRA ~ Condition + (1|Subject), data=hdata)
summary(max_artRRA)
anova(max_artRRA)
m.linear = lm(max_Peak_RRA ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

max_artLMT = art(max_Peak_LMT ~ Condition + (1|Subject), data=hdata)
summary(max_artLMT)
anova(max_artLMT)
m.linear = lm(max_Peak_LMT ~ Condition, data=hdata)
EtaSq(m.linear, type=3)


max_artRMT = art(max_Peak_RMT ~ Condition + (1|Subject), data=hdata)
summary(max_artRMT)
anova(max_artRMT)
m.linear = lm(max_Peak_RMT ~ Condition, data=hdata)
EtaSq(m.linear, type=3)

#vGRF
art_PeakForce_L_N = art(PeakForce_L_N ~ Condition + (1|Subject), data=hdata)
summary(art_PeakForce_L_N)
anova(art_PeakForce_L_N)
m.linear = lm(PeakForce_L_N ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(art_PeakForce_L_N, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

art_PeakForce_R_N = art(PeakForce_R_N ~ Condition + (1|Subject), data=hdata)
summary(art_PeakForce_R_N)
anova(art_PeakForce_R_N)
m.linear = lm(PeakForce_R_N ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(art_PeakForce_R_N, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

art_total_PeakForce_N = art(total_PeakForce_N ~ Condition + (1|Subject), data=hdata)
summary(art_total_PeakForce_N)
anova(art_total_PeakForce_N)
m.linear = lm(total_PeakForce_N ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(art_total_PeakForce_N, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

art_peakForceAsymm_percent = art(peakForceAsymm_percent ~ Condition + (1|Subject), data=hdata)
summary(art_peakForceAsymm_percent)
anova(art_peakForceAsymm_percent)
total_PeakForce_N
m.linear = lm(peakForceAsymm_percent ~ Condition, data=hdata)
EtaSq(m.linear, type=3)


art_meanForceNorm_L_N = art(meanForceNorm_L ~ Condition + (1|Subject), data=hdata)
summary(art_meanForceNorm_L_N)
anova(art_meanForceNorm_L_N)
m.linear = lm(meanForceNorm_L ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(art_meanForceNorm_L_N, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

art_meanForceNorm_R_N = art(meanForceNorm_R ~ Condition + (1|Subject), data=hdata)
summary(art_meanForceNorm_R_N)
anova(art_meanForceNorm_R_N)
m.linear = lm(meanForceNorm_R ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(art_meanForceNorm_R_N, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

art_totalmeanforceNorm = art(totalmeanforceNorm ~ Condition + (1|Subject), data=hdata)
summary(art_totalmeanforceNorm)
anova(art_totalmeanforceNorm)
m.linear = lm(totalmeanforceNorm ~ Condition, data=hdata)
EtaSq(m.linear, type=3)
art.con(art_totalmeanforceNorm, ~Condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
