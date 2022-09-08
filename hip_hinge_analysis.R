#Belt Study Hinge Paper
#written by:  James Kearney & Dr. Joel Martin
#April-July 2022

#Section outline

#1 - load libraries
#2 - load file
#3 - assign variables
#4- Remove Extreme Variables & Transform Data
#5 - explore data 
#6 - MANOVA

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

#-----------------------2 - load file -------------------------------
# load data from spreadsheet
hdata <- read_xlsx("Data_Paper")
my_data<-na.omit(hdata)


#-----------------------3 - assign variables ------------------------
## I BELIEVE YOU NEED TO CHANGE SUBJECT AND CONDITION TO FACTORS
hdata$Subject<-as.factor(hdata$Subject) 
hdata$Condition<-as.factor(hdata$Condition) 

#CHECK THE DATA SET STRUCTURE
str(hdata)

#LABEL IV
hdata$Condition <- factor(hdata$Condition,levels = c(1,2,3,4),labels = c("Control", "Leather belt","Nylon Belt","Vest")) 
#______________________________________________________________________________________________________
#------------------------4-Transform-Data---------------------------------------------------
#BMI to Categories (3 = Yellow = 25-29.9, 2 = Green = 18 - 24.9, 1 = Blue < 17.9)
hdata$BMI<-as.factor(hdata$BMI)
hdata$BMI <- factor(hdata$BMI,levels = c(2,3),labels = c("Green", "Yellow"))

#Remove Extreme Values
#RMS
hdata$NL_RMS_LVL<-winsorize(hdata$NL_RMS_LVL, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                            na.rm = FALSE, type = 7)
hdata$NL_RMS_RVL<-winsorize(hdata$NL_RMS_RVL, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                            na.rm = FALSE, type = 7)
hdata$NL_RMS_LBF<-winsorize(hdata$NL_RMS_LBF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                            na.rm = FALSE, type = 7)
hdata$NL_RMS_RBF<-winsorize(hdata$NL_RMS_RBF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                            na.rm = FALSE, type = 7)
hdata$NL_RMS_LRA<-winsorize(hdata$NL_RMS_LRA, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                            na.rm = FALSE, type = 7)
hdata$NL_RMS_RRA<-winsorize(hdata$NL_RMS_RRA, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                            na.rm = FALSE, type = 7)
hdata$NL_RMS_LMT<-winsorize(hdata$NL_RMS_LMT, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                            na.rm = FALSE, type = 7)
hdata$NL_RMS_RMT<-winsorize(hdata$NL_RMS_RMT, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                            na.rm = FALSE, type = 7)
#Mean
hdata$NL_Mean_LVL<-winsorize(hdata$NL_Mean_LVL, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Mean_RVL<-winsorize(hdata$NL_Mean_RVL, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Mean_LBF<-winsorize(hdata$NL_Mean_LBF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Mean_RBF<-winsorize(hdata$NL_Mean_RBF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Mean_LRA<-winsorize(hdata$NL_Mean_LRA, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Mean_RRA<-winsorize(hdata$NL_Mean_RRA, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Mean_LMT<-winsorize(hdata$NL_Mean_LMT, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Mean_RMT<-winsorize(hdata$NL_Mean_RMT, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
#Peak
hdata$NL_Peak_LVL<-winsorize(hdata$NL_Peak_LVL, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Peak_RVL<-winsorize(hdata$NL_Peak_RVL, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Peak_LBF<-winsorize(hdata$NL_Peak_LBF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Peak_RBF<-winsorize(hdata$NL_Peak_RBF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Peak_LRA<-winsorize(hdata$NL_Peak_LRA, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Peak_RRA<-winsorize(hdata$NL_Peak_RRA, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Peak_LMT<-winsorize(hdata$NL_Peak_LMT, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$NL_Peak_RMT<-winsorize(hdata$NL_Peak_RMT, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
#Peak-Bar
hdata$Br_Peak_LVL<-winsorize(hdata$Br_Peak_LVL, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$Br_Peak_RVL<-winsorize(hdata$Br_Peak_RVL, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$Br_Peak_LBF<-winsorize(hdata$Br_Peak_LBF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$Br_Peak_RBF<-winsorize(hdata$Br_Peak_RBF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$Br_Peak_LRA<-winsorize(hdata$Br_Peak_LRA, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$Br_Peak_RRA<-winsorize(hdata$Br_Peak_RRA, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$Br_Peak_LMT<-winsorize(hdata$Br_Peak_LMT, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
hdata$Br_Peak_RMT<-winsorize(hdata$Br_Peak_RMT, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
#Peak-Max
hdata$max_Peak_LVL<-winsorize(hdata$max_Peak_LVL, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                              na.rm = FALSE, type = 7)
hdata$max_Peak_RVL<-winsorize(hdata$max_Peak_RVL, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                              na.rm = FALSE, type = 7)
hdata$max_Peak_LBF<-winsorize(hdata$max_Peak_LBF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                              na.rm = FALSE, type = 7)
hdata$max_Peak_RBF<-winsorize(hdata$max_Peak_RBF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                              na.rm = FALSE, type = 7)
hdata$max_Peak_LRA<-winsorize(hdata$max_Peak_LRA, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                              na.rm = FALSE, type = 7)
hdata$max_Peak_RRA<-winsorize(hdata$max_Peak_RRA, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                              na.rm = FALSE, type = 7)
hdata$max_Peak_LMT<-winsorize(hdata$max_Peak_LMT, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                              na.rm = FALSE, type = 7)
hdata$max_Peak_RMT<-winsorize(hdata$max_Peak_RMT, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                              na.rm = FALSE, type = 7)
#Remove Subj. 4
hdata2 <- hdata[-c(1, 2, 3, 4), ]
#GRF
hdata2$PeakForce_L_N<-winsorize(hdata2$PeakForce_L_N, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                na.rm = FALSE, type = 7)
hdata2$PeakForce_R_N<-winsorize(hdata2$PeakForce_R_N, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                na.rm = FALSE, type = 7)
hdata2$total_PeakForce_N<-winsorize(hdata2$total_PeakForce_N, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                    na.rm = FALSE, type = 7)
hdata2$peakForceAsymm_percent<-winsorize(hdata2$peakForceAsymm_percent, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                         na.rm = FALSE, type = 7)
hdata2$peakForceNorm_L_BW<-winsorize(hdata2$peakForceNorm_L_BW, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                     na.rm = FALSE, type = 7)
hdata2$peakForceNorm_R_BW<-winsorize(hdata2$peakForceNorm_R_BW, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                     na.rm = FALSE, type = 7)
hdata2$total_PeakForce_BW<-winsorize(hdata2$total_PeakForce_BW, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                     na.rm = FALSE, type = 7)
#----------
hdata2$meanForce_L_N<-winsorize(hdata2$meanForce_L_N, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                na.rm = FALSE, type = 7)
hdata2$meanForce_R_N<-winsorize(hdata2$meanForce_R_N, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                na.rm = FALSE, type = 7)
hdata2$totalMeanForce_N<-winsorize(hdata2$totalMeanForce_N, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                   na.rm = FALSE, type = 7)
hdata2$meanForceAsymm_percent<-winsorize(hdata2$meanForceAsymm_percent, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                         na.rm = FALSE, type = 7)
hdata2$meanForceNorm_L<-winsorize(hdata2$meanForceNorm_L, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                  na.rm = FALSE, type = 7)
hdata2$meanForceNorm_R<-winsorize(hdata2$meanForceNorm_R, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                  na.rm = FALSE, type = 7)
hdata2$totalmeanforceNorm<-winsorize(hdata2$totalmeanforceNorm, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                     na.rm = FALSE, type = 7)


#------------------------5 - explore data --------------------------
#-----------------Hinge-------------------------------------------
#-------------------------------------RMS
boxplot(NL_RMS_LVL~Condition,data=hdata, main="LVL RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out 

boxplot(NL_RMS_RVL~Condition,data=hdata, main="RVL RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_LBF~Condition,data=hdata, main="LBF RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_RBF~Condition,data=hdata, main="RBF RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_LRA~Condition,data=hdata, main="LRA RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_RRA~Condition,data=hdata, main="RRA RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_LMT~Condition,data=hdata, main="LMT RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_RMT~Condition,data=hdata, main="RMT RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

#-----------------------------Mean Frequency-------------------------
boxplot(NL_Mean_LVL~Condition,data=hdata, main="LVL Mean",
        xlab="EMG Mean (mV)", ylab="Condition") 

boxplot(NL_Mean_RVL~Condition,data=hdata, main="RVL Mean",
        xlab="EMG Mean (mV)", ylab="Condition")

boxplot(NL_Mean_LBF~Condition,data=hdata, main="LBF Mean",
        xlab="EMG Mean (mV)", ylab="Condition")

boxplot(NL_Mean_RBF~Condition,data=hdata, main="RBF Mean",
        xlab="EMG Mean (mV)", ylab="Condition")

boxplot(NL_Mean_LRA~Condition,data=hdata, main="LRA Mean",
        xlab="EMG Mean (mV)", ylab="Condition")

boxplot(NL_Mean_RRA~Condition,data=hdata, main="RRA Mean",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_Mean_LMT~Condition,data=hdata, main="LMT Mean",
        xlab="EMG Mean (mV)", ylab="Condition")

boxplot(NL_Mean_RMT~Condition,data=hdata, main="RMT Mean",
        xlab="EMG Mean (mV)", ylab="Condition")

#------------------------------Peak-------------------------------------
boxplot(NL_Peak_LVL~Condition,data=hdata, main="LVL Peak",
        xlab="EMG Peak (mV)", ylab="Condition") 

boxplot(NL_Peak_RVL~Condition,data=hdata, main="RVL Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(NL_Peak_LBF~Condition,data=hdata, main="LBF Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(NL_Peak_RBF~Condition,data=hdata, main="RBF Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(NL_Peak_LRA~Condition,data=hdata, main="LRA Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(NL_Peak_RRA~Condition,data=hdata, main="RRA Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(NL_Peak_LMT~Condition,data=hdata, main="LMT Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(NL_Peak_RMT~Condition,data=hdata, main="RMT Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

#------------------------------Bar Peak-------------------------------------
boxplot(Br_Peak_LVL~Condition,data=hdata, main="LVL Peak",
        xlab="EMG Peak (mV)", ylab="Condition") 

boxplot(Br_Peak_RVL~Condition,data=hdata, main="RVL Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_LBF~Condition,data=hdata, main="LBF Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_RBF~Condition,data=hdata, main="RBF Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_LRA~Condition,data=hdata, main="LRA Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_RRA~Condition,data=hdata, main="RRA Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_LMT~Condition,data=hdata, main="LMT Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_RMT~Condition,data=hdata, main="RMT Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

#------------------------------Max Peak-------------------------------------
boxplot(max_Peak_LVL~Condition,data=hdata, main="LVL Peak",
        xlab="EMG Peak (mV)", ylab="Condition") 

boxplot(max_Peak_RVL~Condition,data=hdata, main="RVL Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_LBF~Condition,data=hdata, main="LBF Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_RBF~Condition,data=hdata, main="RBF Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_LRA~Condition,data=hdata, main="LRA Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_RRA~Condition,data=hdata, main="RRA Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_LMT~Condition,data=hdata, main="LMT Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_RMT~Condition,data=hdata, main="RMT Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

#-------------------------------vGRF---------------------------------

#Peak-----------------------------------------------------------
boxplot(PeakForce_L_N~Condition,data=hdata2, main="Peak Left",
        xlab="vGRF (mN)", ylab="Condition")$out 

boxplot(PeakForce_R_N~Condition,data=hdata2, main="Peak Right",
        xlab="vGRF (mN)", ylab="Condition")$out

boxplot(total_PeakForce_N~Condition,data=hdata2, main="Peak Total",
        xlab="vGRF (mN)", ylab="Condition")$out

boxplot(peakForceAsymm_percent~Condition,data=hdata2, main="Peak Asymm",
        xlab="Percentage", ylab="Condition")$out

#-----%-of-BW-------------------------------------------------------
boxplot(peakForceNorm_L_BW~Condition,data=hdata2, main="Peak Left",
        xlab="Percentage of BW", ylab="Condition")$out 

boxplot(peakForceNorm_R_BW~Condition,data=hdata2, main="Peak Right",
        xlab="Percentage of BW", ylab="Condition")$out

boxplot(total_PeakForce_BW~Condition,data=hdata2, main="Peak Total",
        xlab="Percentage of BW", ylab="Condition")$out

#Mean---------------------------------------------------------
boxplot(meanForce_L_N~Condition,data=hdata2, main="Mean Left",
        xlab="vGRF (mN)", ylab="Condition")$out 

boxplot(meanForce_R_N~Condition,data=hdata2, main="Mean Right",
        xlab="vGRF (mN)", ylab="Condition")$out

boxplot(totalMeanForce_N~Condition,data=hdata2, main="Mean Total",
        xlab="vGRF (mN)", ylab="Condition")$out

boxplot(meanForceAsymm_percent~Condition,data=hdata2, main="Mean Asymm",
        xlab="Percentage", ylab="Condition")$out

#Normalized-Mean------------------------------------------------
boxplot(meanForceNorm_L~Condition,data=hdata2, main="Mean Left",
        xlab="Percentage of BW", ylab="Condition")$out 

boxplot(meanForceNorm_R~Condition,data=hdata2, main="Mean Right",
        xlab="Percentage of BW", ylab="Condition")$out

boxplot(totalmeanforceNorm~Condition,data=hdata2, main="Mean Total",
        xlab="Percentage of BW", ylab="Condition")$out

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
shapiro.test(hdata$Age)
shapiro.test(hdata$Sex)
shapiro.test(hdata$Height)
shapiro.test(hdata$Mass)
shapiro.test(hdata$BMI_V)
shapiro.test(hdata$Comfort)
shapiro.test(hdata$Comfort_Rank)
shapiro.test(hdata$Restrictive)
shapiro.test(hdata$Rstrictive_Rank)

#----------RMS----------------------
shapiro.test(hdata$NL_RMS_LVL)
shapiro.test(hdata$NL_RMS_RVL)
shapiro.test(hdata$NL_RMS_LBF)
shapiro.test(hdata$NL_RMS_RBF)
shapiro.test(hdata$NL_RMS_LRA) 
shapiro.test(hdata$NL_RMS_RRA)
shapiro.test(hdata$NL_RMS_LMT)
shapiro.test(hdata$NL_RMS_RMT)

#---------Mean Frequency------------
shapiro.test(hdata$NL_Mean_LVL)
shapiro.test(hdata$NL_Mean_RVL)
shapiro.test(hdata$NL_Mean_LBF)
shapiro.test(hdata$NL_Mean_RBF)
shapiro.test(hdata$NL_Mean_LRA) 
shapiro.test(hdata$NL_Mean_RRA)  
shapiro.test(hdata$NL_Mean_LMT)
shapiro.test(hdata$NL_Mean_RMT)

#---------Peak----------------------
shapiro.test(hdata$NL_Peak_LVL)
shapiro.test(hdata$NL_Peak_RVL)
shapiro.test(hdata$NL_Peak_LBF)
shapiro.test(hdata$NL_Peak_RBF)
shapiro.test(hdata$NL_Peak_LRA) 
shapiro.test(hdata$NL_Peak_RRA) 
shapiro.test(hdata$NL_Peak_LMT)
shapiro.test(hdata$NL_Peak_RMT)

#---------Peak-Bar---------------------
shapiro.test(hdata$Br_Peak_LVL)
shapiro.test(hdata$Br_Peak_RVL)
shapiro.test(hdata$Br_Peak_LBF)
shapiro.test(hdata$Br_Peak_RBF)
shapiro.test(hdata$Br_Peak_LRA) 
shapiro.test(hdata$Br_Peak_RRA)
shapiro.test(hdata$Br_Peak_LMT)
shapiro.test(hdata$Br_Peak_RMT)

#---------Peak-Max---------------------
shapiro.test(hdata$max_Peak_LVL)
shapiro.test(hdata$max_Peak_RVL)
shapiro.test(hdata$max_Peak_LBF)
shapiro.test(hdata$max_Peak_RBF)
shapiro.test(hdata$max_Peak_LRA) 
shapiro.test(hdata$max_Peak_RRA) 
shapiro.test(hdata$max_Peak_LMT)
shapiro.test(hdata$max_Peak_RMT)

#-------------------------------------vGRF
shapiro.test(hdata2$PeakForce_L_N)
shapiro.test(hdata2$PeakForce_R_N)
shapiro.test(hdata2$total_PeakForce_N)
shapiro.test(hdata2$peakForceAsymm_percent)
shapiro.test(hdata2$peakForceNorm_L_BW)
shapiro.test(hdata2$peakForceNorm_R_BW)
shapiro.test(hdata2$total_PeakForce_BW)
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

hFrMean <- t(hdata[16:23])
mshapiro.test(hFrMean)

hPeak <- t(hdata[24:31])
mshapiro.test(hPeak)

hBar <- t(hdata[32:39])
mshapiro.test(hBar)

hMax <- t(hdata[40:47])
mshapiro.test(hMax)

#--------------------------------------------------------------------------
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
#------------------------Mean Frequency-----------------------------
summaryBy(NL_Mean_LVL~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Mean_RVL~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Mean_LBF~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Mean_RBF~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Mean_LRA~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Mean_RRA~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Mean_LMT~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Mean_RMT~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

#---------------------------Peak---------------------------------
summaryBy(NL_Peak_LVL~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Peak_RVL~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Peak_LBF~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Peak_RBF~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Peak_LRA~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Peak_RRA~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Peak_LMT~ Condition, data = hdata,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_Peak_RMT~ Condition, data = hdata,
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
summaryBy(PeakForce_L_N ~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(PeakForce_R_N ~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(total_PeakForce_N ~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(peakForceAsymm_percent ~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(peakForceNorm_L_BW ~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(peakForceNorm_R_BW ~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(total_PeakForce_BW ~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(meanForce_L_N~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(meanForce_R_N~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(totalMeanForce_N~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(meanForceAsymm_percent~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(meanForceNorm_L~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(meanForceNorm_R~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(totalmeanforceNorm~ Condition, data = hdata2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )
####___________________________________________________________________________________
#------------------------5 - Manova --------------------------
L_RF<-cbind(hdata$NL_RMS_LVL, hdata$NL_Peak_LVL, hdata$NL_Mean_LVL, hdata$Br_Peak_LVL, hdata$max_Peak_LVL)
R_RF<-cbind(hdata$NL_RMS_RVL, hdata$NL_Peak_RVL, hdata$NL_Mean_RVL, hdata$Br_Peak_RVL, hdata$max_Peak_RVL)
L_BF<-cbind(hdata$NL_RMS_LBF, hdata$NL_Peak_LBF, hdata$NL_Mean_LBF, hdata$Br_Peak_LBF, hdata$max_Peak_LBF)
R_BF<-cbind(hdata$NL_RMS_RBF, hdata$NL_Peak_RBF, hdata$NL_Mean_RBF, hdata$Br_Peak_RBF, hdata$max_Peak_RBF)
L_RA<-cbind(hdata$NL_RMS_LRA, hdata$NL_Peak_LRA, hdata$NL_Mean_LRA, hdata$Br_Peak_LRA, hdata$max_Peak_LRA)
R_RA<-cbind(hdata$NL_RMS_RRA, hdata$NL_Peak_RRA, hdata$NL_Mean_RRA, hdata$Br_Peak_RRA, hdata$max_Peak_RRA)
L_MT<-cbind(hdata$NL_RMS_LMT, hdata$NL_Peak_LMT, hdata$NL_Mean_LMT, hdata$Br_Peak_LMT, hdata$max_Peak_LMT)
R_MT<-cbind(hdata$NL_RMS_RMT, hdata$NL_Peak_RMT, hdata$NL_Mean_RMT, hdata$Br_Peak_RMT, hdata$max_Peak_RMT)

Model1<-nonpartest(L_RF ~ Condition, data = hdata ,permreps=1000)
Model1 

#Sig-------------------------------^^-------p<0.001--------------
kruskal.test(NL_RMS_LVL ~ Condition, data = hdata)
dunn_test(hdata, NL_RMS_LVL ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Mean_LVL ~ Condition, data = hdata)
dunn_test(hdata, NL_Mean_LVL ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Peak_LVL ~ Condition, data = hdata)
dunn_test(hdata, NL_Peak_LVL ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(Br_Peak_LVL ~ Condition, data = hdata)
dunn_test(hdata, Br_Peak_LVL ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(max_Peak_LVL ~ Condition, data = hdata)
dunn_test(hdata, max_Peak_LVL ~ Condition, p.adjust.method = "none", detailed = FALSE)
#------------------------------------------------------------------------------------

#-------------------------------------------------------
Model2<-nonpartest(R_RF ~ Condition, data = hdata ,permreps=1000)
Model2 

#--------^Sig------------------------------------------------------------------------
kruskal.test(NL_RMS_RVL ~ Condition, data = hdata)
dunn_test(hdata, NL_RMS_RVL ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Mean_RVL ~ Condition, data = hdata)
dunn_test(hdata, NL_Mean_RVL ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Peak_RVL ~ Condition, data = hdata)
dunn_test(hdata, NL_Peak_RVL ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(Br_Peak_RVL ~ Condition, data = hdata)
dunn_test(hdata, Br_Peak_RVL ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(max_Peak_RVL ~ Condition, data = hdata)
dunn_test(hdata, max_Peak_RVL ~ Condition, p.adjust.method = "none", detailed = FALSE)
#------------------------------------------------------------------------------------ 

#-------------------------------------------------------------
Model3<-nonpartest(L_BF ~ Condition, data = hdata ,permreps=1000)
Model3
#----------------------------Sig------------------------------------------------------
kruskal.test(NL_RMS_LBF ~ Condition, data = hdata)
dunn_test(hdata, NL_RMS_LBF ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Mean_LBF ~ Condition, data = hdata)
dunn_test(hdata, NL_Mean_LBF ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Peak_LBF ~ Condition, data = hdata)
dunn_test(hdata, NL_Peak_LBF ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(Br_Peak_LBF ~ Condition, data = hdata)
dunn_test(hdata, Br_Peak_LBF ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(max_Peak_LBF ~ Condition, data = hdata)
dunn_test(hdata, max_Peak_LBF ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig
#-------------------------------------------------------------------------------------
#--------------------------------------------
Model4<-nonpartest(R_BF ~ Condition, data = hdata ,permreps=1000)
Model4
#--------^Not Sig--------------------------------------------------------------------

Model5<-nonpartest(L_RA ~ Condition, data = hdata ,permreps=1000)
Model5
#---------------------------------------^^^----Sig-----------------------------------
kruskal.test(NL_RMS_LRA ~ Condition, data = hdata)
dunn_test(hdata, NL_RMS_LRA ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Mean_LRA ~ Condition, data = hdata)
dunn_test(hdata, NL_Mean_LRA ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Peak_LRA ~ Condition, data = hdata)
dunn_test(hdata, NL_Peak_LRA ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(Br_Peak_LRA ~ Condition, data = hdata)
dunn_test(hdata, Br_Peak_LRA ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(max_Peak_LRA ~ Condition, data = hdata)
dunn_test(hdata, max_Peak_LRA ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig
#--------------------------------------------------------------------------------------

#---------------------------------------------------------------
Model6<-nonpartest(R_RA ~ Condition, data = hdata ,permreps=1000)
Model6
#--------------------------------------Not-Sig-------------------

#---------------------------------------------------------------
Model7<-nonpartest(L_MT ~ Condition, data = hdata ,permreps=1000)
Model7
#-------------------------^ Sig---------------------------------

kruskal.test(NL_RMS_LMT ~ Condition, data = hdata)
dunn_test(hdata, NL_RMS_LMT ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Mean_LMT ~ Condition, data = hdata)
dunn_test(hdata, NL_Mean_LMT ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Peak_LMT ~ Condition, data = hdata)
dunn_test(hdata, NL_Peak_LMT ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(Br_Peak_LMT ~ Condition, data = hdata)
dunn_test(hdata, Br_Peak_LMT ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(max_Peak_LMT ~ Condition, data = hdata)
dunn_test(hdata, max_Peak_LMT ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig
#-----------------------------------------------------------------------------

#-----------------------------------------------------------
Model8<-nonpartest(R_MT ~ Condition, data = hdata ,permreps=1000)
Model8
#----------------------------------------------------------

kruskal.test(NL_RMS_RMT ~ Condition, data = hdata)
dunn_test(hdata, NL_RMS_RMT ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Mean_RMT ~ Condition, data = hdata)
dunn_test(hdata, NL_Mean_RMT ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Peak_RMT ~ Condition, data = hdata)
dunn_test(hdata, NL_Peak_RMT ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(Br_Peak_RMT ~ Condition, data = hdata)
dunn_test(hdata, Br_Peak_RMT ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(max_Peak_RMT ~ Condition, data = hdata)
dunn_test(hdata, max_Peak_RMT ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig
#------------------------------------------------------------------------------
#_______________________________________________________________________________________________
#vGRF
#------------------Binding------------------------------------------
Hinge_GRF<-cbind(hdata2$PeakForce_L_N, hdata2$PeakForce_R_N, hdata2$total_PeakForce_N, hdata2$peakForceAsymm_percent,
                 hdata2$peakForceNorm_L_BW, hdata2$peakForceNorm_R_BW, hdata2$total_PeakForce_BW, hdata2$meanForce_L_N, 
                 hdata2$meanForce_R_N, hdata2$totalMeanForce_N, hdata2$meanForceAsymm_percent, hdata2$meanForceNorm_L, 
                 hdata2$meanForceNorm_R, hdata2$totalmeanforceNorm) 

Hinge_GRF_1<-cbind(hdata2$PeakForce_L_N, hdata2$PeakForce_R_N, hdata2$total_PeakForce_N, hdata2$peakForceAsymm_percent,
                   hdata2$peakForceNorm_L_BW, hdata2$peakForceNorm_R_BW, hdata2$total_PeakForce_BW)

Hinge_GRF_2<-cbind(hdata2$meanForce_L_N, hdata2$meanForce_R_N, hdata2$totalMeanForce_N, hdata2$meanForceAsymm_percent, 
                   hdata2$meanForceNorm_L, hdata2$meanForceNorm_R, hdata2$totalmeanforceNorm)

#--------------------------MANOVAS-----------------------------------------------------------------
Model9<-nonpartest(Hinge_GRF ~ Condition, data = hdata2 ,permreps=1000)
Model9
#---------------------------------Sig-------------------------

Model10<-nonpartest(Hinge_GRF_1 ~ Condition, data = hdata2 ,permreps=1000)
Model10
#--------------------------------Sig-------------------------

#--------------------------------------------------------------
Model11<-nonpartest(Hinge_GRF_2 ~ Condition, data = hdata2 ,permreps=1000)
Model11
#-----------------------------Not-Sig--------------------------

kruskal.test(PeakForce_L_N ~ Condition, data = hdata2)
dunn_test(hdata2, PeakForce_L_N ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(PeakForce_R_N ~ Condition, data = hdata2)
dunn_test(hdata2, PeakForce_R_N ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(total_PeakForce_N ~ Condition, data = hdata2)
dunn_test(hdata2, total_PeakForce_N ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(peakForceAsymm_percent ~ Condition, data = hdata2)
dunn_test(hdata2, peakForceAsymm_percent ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(peakForceNorm_L_BW ~ Condition, data = hdata2)
dunn_test(hdata2, peakForceNorm_L_BW ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(peakForceNorm_R_BW ~ Condition, data = hdata2)
dunn_test(hdata2, peakForceNorm_R_BW ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(total_PeakForce_BW ~ Condition, data = hdata2)
dunn_test(hdata2, total_PeakForce_BW ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

#-----------------------------------------------------------------------------------------
kruskal.test(meanForce_L_N ~ Condition, data = hdata2)
dunn_test(hdata2, meanForce_L_N ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(meanForce_R_N ~ Condition, data = hdata2)
dunn_test(hdata2, meanForce_R_N ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(totalMeanForce_N ~ Condition, data = hdata2)
dunn_test(hdata2, totalMeanForce_N ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(meanForceAsymm_percent ~ Condition, data = hdata2)
dunn_test(hdata2, meanForceAsymm_percent ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(meanForceNorm_L ~ Condition, data = hdata2)
dunn_test(hdata2, meanForceNorm_L ~ Condition, p.adjust.method = "none", detailed = FALSE)

kruskal.test(meanForceNorm_R ~ Condition, data = hdata2)
dunn_test(hdata2, meanForceNorm_R ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

kruskal.test(totalmeanforceNorm ~ Condition, data = hdata2)
dunn_test(hdata2, totalmeanforceNorm ~ Condition, p.adjust.method = "none", detailed = FALSE)
#Not Sig

#--------------------------------------------------------------------------------------------------
#________________________________________________________________________________________________________-
#Demographics
#--------------------A-Quick-Look--------------------------
Demographics<-cbind(hdata$Age, hdata$Sex, hdata$Height, hdata$Mass, hdata$BMI)

ModelDemographics <- manova(Demographics ~ Condition, data=hdata)
summary(ModelDemographics, intercept = TRUE, test="Wilks")
#-Not-Sig------------------------------------
#------------------------Demographics-By-Muscle---------------------
#--------------Control----------------
ControlOne<- hdata[1:1, ] 
ControlTwo<- hdata[5:5, ]
ControlThree<- hdata[9:9, ]
ControlFour<- hdata[13:13, ]
ControlFive<- hdata[17:17, ]
ControlSix<- hdata[21:21, ]
ControlSeven<- hdata[25:25, ]
ControlEight<- hdata[29:29, ]
ControlNine<- hdata[33:33, ]
ControlTen<- hdata[37:37, ]
ControlEleven<- hdata[41:41, ]
ControlTwelve<- hdata[45:45, ]
ControlThirteen<- hdata[49:49, ]
ControlFourteen<- hdata[53:53, ]
ControlFifteen<- hdata[57:57, ]
ControlSixteen<- hdata[61:61, ]
ControlSeventeen<- hdata[65:65, ]
ControlEighteen<- hdata[69:69, ]
ControlNineteen<- hdata[73:73, ]
ControlTwenty<- hdata[77:77, ]
ControlTwentyOne<- hdata[81:81, ]
ControlTwentyTwo<- hdata[85:85, ]
ControlTwentyThree<- hdata[89:89, ]
ControlTwentyFour<- hdata[93:93, ]
ControlTwentyFive<- hdata[97:97, ]
ControlCondition<- rbind(ControlOne, ControlTwo, ControlThree, ControlFour, ControlFive, ControlSix, 
                         ControlSeven, ControlEight, ControlNine, ControlTen, ControlEleven,ControlTwelve,
                         ControlThirteen, ControlFourteen, ControlFifteen, ControlSixteen, ControlSeventeen,
                         ControlEighteen, ControlNineteen, ControlTwenty, ControlTwentyOne, ControlTwentyTwo,
                         ControlTwentyThree, ControlTwentyFour,ControlTwentyFive)

#---------------Leather-----------------
LeatherOne<- hdata[2:2, ] 
LeatherTwo<- hdata[6:6, ]
LeatherThree<- hdata[10:10, ]
LeatherFour<- hdata[14:14, ]
LeatherFive<- hdata[18:18, ]
LeatherSix<- hdata[22:22, ]
LeatherSeven<- hdata[26:26, ]
LeatherEight<- hdata[30:30, ]
LeatherNine<- hdata[34:34, ]
LeatherTen<- hdata[38:38, ]
LeatherEleven<- hdata[42:42, ]
LeatherTwelve<- hdata[46:46, ]
LeatherThirteen<- hdata[50:50, ]
LeatherFourteen<- hdata[54:54, ]
LeatherFifteen<- hdata[58:58, ]
LeatherSixteen<- hdata[62:62, ]
LeatherSeventeen<- hdata[66:66, ]
LeatherEighteen<- hdata[70:70, ]
LeatherNineteen<- hdata[74:74, ]
LeatherTwenty<- hdata[78:78, ]
LeatherTwentyOne<- hdata[82:82, ]
LeatherTwentyTwo<- hdata[86:86, ]
LeatherTwentyThree<- hdata[90:90, ]
LeatherTwentyFour<- hdata[94:94, ]
LeatherTwentyFive<- hdata[98:98, ]
LeatherCondition<- rbind(LeatherOne, LeatherTwo, LeatherThree, LeatherFour, LeatherFive, LeatherSix, 
                         LeatherSeven, LeatherEight, LeatherNine, LeatherTen, LeatherEleven,LeatherTwelve,
                         LeatherThirteen, LeatherFourteen, LeatherFifteen, LeatherSixteen, LeatherSeventeen,
                         LeatherEighteen, LeatherNineteen, LeatherTwenty, LeatherTwentyOne, LeatherTwentyTwo,
                         LeatherTwentyThree, LeatherTwentyFour, LeatherTwentyFive)

#---------------Nylon-----------------
NylonOne<- hdata[3:3, ] 
NylonTwo<- hdata[7:7, ]
NylonThree<- hdata[11:11, ]
NylonFour<- hdata[15:15, ]
NylonFive<- hdata[19:19, ]
NylonSix<- hdata[23:23, ]
NylonSeven<- hdata[27:27, ]
NylonEight<- hdata[31:31, ]
NylonNine<- hdata[35:35, ]
NylonTen<- hdata[39:39, ]
NylonEleven<- hdata[43:43, ]
NylonTwelve<- hdata[47:47, ]
NylonThirteen<- hdata[51:51, ]
NylonFourteen<- hdata[55:55, ]
NylonFifteen<- hdata[59:59, ]
NylonSixteen<- hdata[63:63, ]
NylonSeventeen<- hdata[67:67, ]
NylonEighteen<- hdata[71:71, ]
NylonNineteen<- hdata[75:75, ]
NylonTwenty<- hdata[79:79, ]
NylonTwentyOne<- hdata[83:83, ]
NylonTwentyTwo<- hdata[87:87, ]
NylonTwentyThree<- hdata[91:91, ]
NylonTwentyFour<- hdata[95:95, ]
NylonTwentyFive<- hdata[99:99, ]
NylonCondition<- rbind(NylonOne, NylonTwo, NylonThree, NylonFour, NylonFive, NylonSix, 
                       NylonSeven, NylonEight, NylonNine, NylonTen, NylonEleven,NylonTwelve,
                       NylonThirteen, NylonFourteen, NylonFifteen, NylonSixteen, NylonSeventeen,
                       NylonEighteen, NylonNineteen, NylonTwenty, NylonTwentyOne, NylonTwentyTwo,
                       NylonTwentyThree, NylonTwentyFour, NylonTwentyFive)

#---------------Vest-----------------
VestOne<- hdata[4:4, ] 
VestTwo<- hdata[8:8, ]
VestThree<- hdata[12:12, ]
VestFour<- hdata[16:16, ]
VestFive<- hdata[20:20, ]
VestSix<- hdata[24:24, ]
VestSeven<- hdata[28:28, ]
VestEight<- hdata[32:32, ]
VestNine<- hdata[36:36, ]
VestTen<- hdata[40:40, ]
VestEleven<- hdata[44:44, ]
VestTwelve<- hdata[48:48, ]
VestThirteen<- hdata[52:52, ]
VestFourteen<- hdata[56:56, ]
VestFifteen<- hdata[60:60, ]
VestSixteen<- hdata[64:64, ]
VestSeventeen<- hdata[68:68, ]
VestEighteen<- hdata[72:72, ]
VestNineteen<- hdata[76:76, ]
VestTwenty<- hdata[80:80, ]
VestTwentyOne<- hdata[84:84, ]
VestTwentyTwo<- hdata[88:88, ]
VestTwentyThree<- hdata[92:92, ]
VestTwentyFour<- hdata[96:96, ]
VestTwentyFive<- hdata[100:100, ]
VestCondition<- rbind(VestOne, VestTwo, VestThree, VestFour, VestFive, VestSix, 
                      VestSeven, VestEight, VestNine, VestTen, VestEleven, VestTwelve,
                      VestThirteen, VestFourteen, VestFifteen, VestSixteen, VestSeventeen,
                      VestEighteen, VestNineteen, VestTwenty, VestTwentyOne, VestTwentyTwo,
                      VestTwentyThree, VestTwentyFour, VestTwentyFive)
#----------------------------------------------------------------------------
#------------------------Binding Variables-----------------------------------
#L_ = Leather, N_ = Nylon, V_ = Vest
#------------------Leather-------------------------
L_Hinge_RMS<-cbind(LeatherCondition$NL_RMS_LVL, LeatherCondition$NL_RMS_RVL, LeatherCondition$NL_RMS_LBF, 
                   LeatherCondition$NL_RMS_RBF, LeatherCondition$NL_RMS_LRA, LeatherCondition$NL_RMS_RRA, 
                   LeatherCondition$NL_RMS_LMT, LeatherCondition$NL_RMS_RMT)

L_Hinge_Mean<-cbind(LeatherCondition$NL_Mean_LVL, LeatherCondition$NL_Mean_RVL, LeatherCondition$NL_Mean_LBF, 
                    LeatherCondition$NL_Mean_RBF, LeatherCondition$NL_Mean_LRA, LeatherCondition$NL_Mean_RRA, 
                    LeatherCondition$NL_Mean_LMT, LeatherCondition$NL_Mean_RMT)

L_Hinge_Peak<-cbind(LeatherCondition$NL_Peak_LVL,  LeatherCondition$NL_Peak_RVL, LeatherCondition$NL_Peak_LBF, 
                    LeatherCondition$NL_Peak_RBF, LeatherCondition$NL_Peak_LRA, LeatherCondition$NL_Peak_RRA, 
                    LeatherCondition$NL_Peak_LMT, LeatherCondition$NL_Peak_RMT)

L_BrHinge_Peak<-cbind(LeatherCondition$Br_Peak_LVL, LeatherCondition$Br_Peak_RVL, LeatherCondition$Br_Peak_LBF, 
                      LeatherCondition$Br_Peak_RBF, LeatherCondition$Br_Peak_LRA, LeatherCondition$Br_Peak_RRA, 
                      LeatherCondition$Br_Peak_LMT, LeatherCondition$Br_Peak_RMT)

L_maxHinge_Peak<-cbind(LeatherCondition$max_Peak_LVL, LeatherCondition$max_Peak_RVL, LeatherCondition$max_Peak_LBF, 
                       LeatherCondition$max_Peak_RBF, LeatherCondition$max_Peak_LRA, LeatherCondition$max_Peak_RRA, 
                       LeatherCondition$max_Peak_LMT, LeatherCondition$max_Peak_RMT)
#------------------Nylon-------------------------
N_Hinge_RMS<-cbind(NylonCondition$NL_RMS_LVL, NylonCondition$NL_RMS_RVL, NylonCondition$NL_RMS_LBF, 
                   NylonCondition$NL_RMS_RBF, NylonCondition$NL_RMS_LRA, NylonCondition$NL_RMS_RRA, 
                   NylonCondition$NL_RMS_LMT, NylonCondition$NL_RMS_RMT)

N_Hinge_Mean<-cbind(NylonCondition$NL_Mean_LVL, NylonCondition$NL_Mean_RVL, NylonCondition$NL_Mean_LBF, 
                    NylonCondition$NL_Mean_RBF, NylonCondition$NL_Mean_LRA, NylonCondition$NL_Mean_RRA, 
                    NylonCondition$NL_Mean_LMT, NylonCondition$NL_Mean_RMT)

N_Hinge_Peak<-cbind(NylonCondition$NL_Peak_LVL, NylonCondition$NL_Peak_RVL, NylonCondition$NL_Peak_LBF, 
                    NylonCondition$NL_Peak_RBF, NylonCondition$NL_Peak_LRA, NylonCondition$NL_Peak_RRA, 
                    NylonCondition$NL_Peak_LMT, NylonCondition$NL_Peak_RMT)

N_BrHinge_Peak<-cbind(NylonCondition$Br_Peak_LVL, NylonCondition$Br_Peak_RVL, NylonCondition$Br_Peak_LBF, 
                      NylonCondition$Br_Peak_RBF, NylonCondition$Br_Peak_LRA, NylonCondition$Br_Peak_RRA, 
                      NylonCondition$Br_Peak_LMT, NylonCondition$Br_Peak_RMT)

N_maxHinge_Peak<-cbind(NylonCondition$max_Peak_LVL, NylonCondition$max_Peak_RVL, NylonCondition$max_Peak_LBF, 
                       NylonCondition$max_Peak_RBF, NylonCondition$max_Peak_LRA, NylonCondition$max_Peak_RRA, 
                       NylonCondition$max_Peak_LMT, NylonCondition$max_Peak_RMT)
#------------------Vest-------------------------
V_Hinge_RMS<-cbind(VestCondition$NL_RMS_LVL, VestCondition$NL_RMS_RVL, VestCondition$NL_RMS_LBF, 
                   VestCondition$NL_RMS_RBF, VestCondition$NL_RMS_LRA, VestCondition$NL_RMS_RRA, 
                   VestCondition$NL_RMS_LMT, VestCondition$NL_RMS_RMT)

V_Hinge_Mean<-cbind(VestCondition$NL_Mean_LVL, VestCondition$NL_Mean_RVL, VestCondition$NL_Mean_LBF, 
                    VestCondition$NL_Mean_RBF, VestCondition$NL_Mean_LRA, VestCondition$NL_Mean_RRA, 
                    VestCondition$NL_Mean_LMT, VestCondition$NL_Mean_RMT)

V_Hinge_Peak<-cbind(NylonCondition$NL_Peak_LVL, NylonCondition$NL_Peak_RVL, NylonCondition$NL_Peak_LBF, 
                    NylonCondition$NL_Peak_RBF, NylonCondition$NL_Peak_LRA, NylonCondition$NL_Peak_RRA, 
                    NylonCondition$NL_Peak_LMT, NylonCondition$NL_Peak_RMT)

V_BrHinge_Peak<-cbind(NylonCondition$Br_Peak_LVL, NylonCondition$Br_Peak_RVL, NylonCondition$Br_Peak_LBF, 
                      NylonCondition$Br_Peak_RBF, NylonCondition$Br_Peak_LRA, NylonCondition$Br_Peak_RRA, 
                      NylonCondition$Br_Peak_LMT, NylonCondition$Br_Peak_RMT)

V_maxHinge_Peak<-cbind(NylonCondition$max_Peak_LVL, NylonCondition$max_Peak_RVL, NylonCondition$max_Peak_LBF, 
                       NylonCondition$max_Peak_RBF, NylonCondition$max_Peak_LRA, NylonCondition$max_Peak_RRA, 
                       NylonCondition$max_Peak_LMT, NylonCondition$max_Peak_RMT)

#------------------------Anova's-of-Demographics-By-Condition
#-------------Leather-----------------------------------------------
#------------------------------BMI-----------------------------------
LeatherBMIRMS <- manova(L_Hinge_RMS ~ BMI, data=LeatherCondition)
summary(LeatherBMIRMS)

LeatherBMIMean <- manova(L_Hinge_Mean ~ BMI, data=LeatherCondition)
summary(LeatherBMIMean)

LeatherBMIPeak <- manova(L_Hinge_Peak ~ BMI, data=LeatherCondition)
summary(LeatherBMIPeak)

LeatherBMIBar <- manova(L_BrHinge_Peak ~ BMI, data=LeatherCondition)
summary(LeatherBMIBar)

LeatherBMIMax <- manova(L_maxHinge_Peak ~ BMI, data=LeatherCondition)
summary(LeatherBMIMax)

#------------------------------Age-----------------------------------
LeatherAgeRMS <- manova(L_Hinge_RMS ~ Age, data=LeatherCondition)
summary(LeatherAgeRMS)

LeatherAgeMean <- manova(L_Hinge_Mean ~ Age, data=LeatherCondition)
summary(LeatherAgeMean)

LeatherAgePeak <- manova(L_Hinge_Peak ~ Age, data=LeatherCondition)
summary(LeatherAgePeak)

LeatherAgeBar <- manova(L_BrHinge_Peak ~ Age, data=LeatherCondition)
summary(LeatherAgeBar)

LeatherAgeMax <- manova(L_maxHinge_Peak ~ Age, data=LeatherCondition)
summary(LeatherAgeMax)

#------------------------------Mass-----------------------------------
LeatherMassRMS <- manova(L_Hinge_RMS ~ Mass, data=LeatherCondition)
summary(LeatherMassRMS)

LeatherMassMean <- manova(L_Hinge_Mean ~ Mass, data=LeatherCondition)
summary(LeatherMassMean)

LeatherMassPeak <- manova(L_Hinge_Peak ~ Mass, data=LeatherCondition)
summary(LeatherMassPeak)

LeatherMassBar <- manova(L_BrHinge_Peak ~ Mass, data=LeatherCondition)
summary(LeatherMassBar)

LeatherMassMax <- manova(L_maxHinge_Peak ~ Mass, data=LeatherCondition)
summary(LeatherMassMax)
#------------------------------Height-----------------------------------
LeatherHeightRMS <- manova(L_Hinge_RMS ~ Height, data=LeatherCondition)
summary(LeatherHeightRMS)

LeatherHeightMean <- manova(L_Hinge_Mean ~ Height, data=LeatherCondition)
summary(LeatherHeightMean)

LeatherHeightPeak <- manova(L_Hinge_Peak ~ Height, data=LeatherCondition)
summary(LeatherHeightPeak)

LeatherHeightBar <- manova(L_BrHinge_Peak ~ Height, data=LeatherCondition)
summary(LeatherHeightBar)

LeatherHeightMax <- manova(L_maxHinge_Peak ~ Height, data=LeatherCondition)
summary(LeatherHeightMax)
#------------------------------Sex-------------------------------------
LeatherSexRMS <- manova(L_Hinge_RMS ~ Sex, data=LeatherCondition)
summary(LeatherSexRMS)

LeatherSexMean <- manova(L_Hinge_Mean ~ Sex, data=LeatherCondition)
summary(LeatherSexMean)

LeatherSexPeak <- manova(L_Hinge_Peak ~ Sex, data=LeatherCondition)
summary(LeatherSexPeak)

LeatherSexBar <- manova(L_BrHinge_Peak ~ Sex, data=LeatherCondition)
summary(LeatherSexBar)

LeatherSexMax <- manova(L_maxHinge_Peak ~ Sex, data=LeatherCondition)
summary(LeatherSexMax)

#-------------Nylon-----------------------------------------------

#------------------------------BMI-----------------------------------
NylonBMIRMS <- manova(N_Hinge_RMS ~ BMI, data=NylonCondition)
summary(NylonBMIRMS)

NylonBMIMean <- manova(N_Hinge_Mean ~ BMI, data=NylonCondition)
summary(NylonBMIMean)

NylonBMIPeak <- manova(N_Hinge_Peak ~ BMI, data=NylonCondition)
summary(NylonBMIPeak)

NylonBMIBar <- manova(N_BrHinge_Peak ~ BMI, data=NylonCondition)
summary(NylonBMIBar)

NylonBMIMax <- manova(N_maxHinge_Peak ~ BMI, data=NylonCondition)
summary(NylonBMIMax)
summary.aov(NylonBMIMax)
#Sig
#-------------------------------------------------------------------

NylonBMIPeak.4 <- aov(max_Peak_RBF ~ BMI, data=LeatherCondition)
summary(NylonBMIPeak.4)
TukeyHSD(NylonBMIPeak.4)
#Not Sig
#------------------------------------------------------------------

#------------------------------Age-----------------------------------
NylonAgeRMS <- manova(N_Hinge_RMS ~ Age, data=NylonCondition)
summary(NylonAgeRMS)

NylonAgeMean <- manova(N_Hinge_Mean ~ Age, data=NylonCondition)
summary(NylonAgeMean)

NylonAgePeak <- manova(N_Hinge_Peak ~ Age, data=NylonCondition)
summary(NylonAgePeak)

NylonAgeBar <- manova(N_BrHinge_Peak ~ Age, data=NylonCondition)
summary(NylonAgeBar)

NylonAgeMax <- manova(N_maxHinge_Peak ~ Age, data=NylonCondition)
summary(NylonAgeMax)

#------------------------------Mass-----------------------------------
NylonMassRMS <- manova(N_Hinge_RMS ~ Mass, data=LeatherCondition)
summary(LeatherMassRMS)

NylonMassMean <- manova(N_Hinge_Mean ~ Mass, data=NylonCondition)
summary(NylonMassMean)

NylonMassPeak <- manova(N_Hinge_Peak ~ Mass, data=NylonCondition)
summary(NylonMassPeak)

NylonMassBar <- manova(N_BrHinge_Peak ~ Mass, data=NylonCondition)
summary(NylonMassBar)

NylonMassMax <- manova(N_maxHinge_Peak ~ Mass, data=NylonCondition)
summary(NylonMassMax)

#------------------------------Height-----------------------------------
NylonHeightRMS <- manova(N_Hinge_RMS ~ Height, data=NylonCondition)
summary(NylonHeightRMS)

NylonHeightMean <- manova(N_Hinge_Mean ~ Height, data=NylonCondition)
summary(NylonHeightMean)

NylonHeightPeak <- manova(N_Hinge_Peak ~ Height, data=NylonCondition)
summary(NylonHeightPeak)

NylonHeightBar <- manova(N_BrHinge_Peak ~ Height, data=NylonCondition)
summary(NylonHeightBar)

NylonHeightMax <- manova(N_maxHinge_Peak ~ Height, data=NylonCondition)
summary(NylonHeightMax)

#------------------------------Sex-------------------------------------
NylonSexRMS <- manova(N_Hinge_RMS ~ Sex, data=NylonCondition)
summary(NylonSexRMS)

NylonSexMean <- manova(N_Hinge_Mean ~ Sex, data=NylonCondition)
summary(NylonSexMean)

NylonSexPeak <- manova(N_Hinge_Peak ~ Sex, data=NylonCondition)
summary(NylonSexPeak)

NylonSexBar <- manova(N_BrHinge_Peak ~ Sex, data=NylonCondition)
summary(NylonSexBar)

NylonSexMax <- manova(N_maxHinge_Peak ~ Sex, data=NylonCondition)
summary(NylonSexMax)

#-------------Vest-----------------------------------------------

#------------------------------BMI-----------------------------------
VestBMIRMS <- manova(V_Hinge_RMS ~ BMI, data=VestCondition)
summary(VestBMIRMS)

VestBMIMean <- manova(V_Hinge_Mean ~ BMI, data=VestCondition)
summary(VestBMIMean)

VestBMIPeak <- manova(V_Hinge_Peak ~ BMI, data=VestCondition)
summary(VestBMIPeak)

VestBMIBar <- manova(V_BrHinge_Peak ~ BMI, data=VestCondition)
summary(VestBMIBar)

VestBMIMax <- manova(V_maxHinge_Peak ~ BMI, data=VestCondition)
summary(VestBMIMax)
summary.aov(VestBMIMax)
#Sig
#----------------------------------------------------------
VestBMIMax.1 <- aov(max_Peak_RBF ~ BMI, data=VestCondition)
summary(VestBMIMax.1)
#Not Sig---------------------------------------------------

#------------------------------Age-----------------------------------
VestAgeRMS <- manova(V_Hinge_RMS ~ Age, data=VestCondition)
summary(VestAgeRMS)
summary.aov(VestAgeRMS) #Not Sig
#Sig

VestAgeMean <- manova(V_Hinge_Mean ~ Age, data=VestCondition)
summary(VestAgeMean)

VestAgePeak <- manova(V_Hinge_Peak ~ Age, data=VestCondition)
summary(VestAgePeak)

VestAgeBar <- manova(V_BrHinge_Peak ~ Age, data=VestCondition)
summary(VestAgeBar)

VestAgeMax <- manova(V_maxHinge_Peak ~ Age, data=VestCondition)
summary(VestAgeMax)

#------------------------------Mass-----------------------------------
VestMassRMS <- manova(V_Hinge_RMS ~ Mass, data=VestCondition)
summary(VestMassRMS)

VestMassMean <- manova(V_Hinge_Mean ~ Mass, data=VestCondition)
summary(VestMassMean)

VestMassPeak <- manova(V_Hinge_Peak ~ Mass, data=VestCondition)
summary(VestMassPeak)

VestMassBar <- manova(V_BrHinge_Peak ~ Mass, data=VestCondition)
summary(VestMassBar)

VestMassMax <- manova(V_maxHinge_Peak ~ Mass, data=VestCondition)
summary(LeatherMassMax)

#------------------------------Height-----------------------------------
VestHeightRMS <- manova(V_Hinge_RMS ~ Height, data=VestCondition)
summary(VestHeightRMS)

VestHeightMean <- manova(V_Hinge_Mean ~ Height, data=VestCondition)
summary(VestHeightMean)

VestHeightPeak <- manova(V_Hinge_Peak ~ Height, data=VestCondition)
summary(VestHeightPeak)

VestHeightBar <- manova(V_BrHinge_Peak ~ Height, data=VestCondition)
summary(VestHeightBar)

VestHeightMax <- manova(V_maxHinge_Peak ~ Height, data=VestCondition)
summary(VestHeightMax)

#------------------------------Sex-------------------------------------
VestSexRMS <- manova(V_Hinge_RMS ~ Sex, data=VestCondition)
summary(VestSexRMS)

VestSexMean <- manova(V_Hinge_Mean ~ Sex, data=VestCondition)
summary(VestSexMean)

VestSexPeak <- manova(V_Hinge_Peak ~ Sex, data=VestCondition)
summary(VestSexPeak)

VestSexBar <- manova(V_BrHinge_Peak ~ Sex, data=VestCondition)
summary(VestSexBar)

VestSexMax <- manova(V_maxHinge_Peak ~ Sex, data=VestCondition)
summary(VestSexMax)
#_____________________________________________________________________________________________
#-----------------Effect-Size-(Cliff's-Delta)-For-Sig-Variables---------------------
#------------------LRF------------------------------------------------------------
#RMS-Leather
delta_1.1<-cliff.delta(d = LeatherCondition$NL_RMS_LVL,
                       f = ControlCondition$NL_RMS_LVL)
delta_1.1

#RMS-Nylon
delta_1.2<-cliff.delta(d = NylonCondition$NL_RMS_LVL,
                       f = ControlCondition$NL_RMS_LVL)
delta_1.2

#RMS-Vest
delta_1.3<-cliff.delta(d = VestCondition$NL_RMS_LVL,
                       f = ControlCondition$NL_RMS_LVL)
delta_1.3

#Mean-Leather
delta_1.4<-cliff.delta(d = LeatherCondition$NL_Mean_LVL,
                       f = ControlCondition$NL_Mean_LVL)
delta_1.4

#Mean-Nylon
delta_1.5<-cliff.delta(d = NylonCondition$NL_Mean_LVL,
                       f = ControlCondition$NL_Mean_LVL)
delta_1.5

#Mean-Vest
delta_1.6<-cliff.delta(d = VestCondition$NL_Mean_LVL,
                       f = ControlCondition$NL_Mean_LVL)
delta_1.6

#Bar-Leather
delta_1.7<-cliff.delta(d = LeatherCondition$Br_Peak_LVL,
                       f = ControlCondition$Br_Peak_LVL)
delta_1.7

#Bar-Nylon
delta_1.8<-cliff.delta(d = NylonCondition$Br_Peak_LVL,
                       f = ControlCondition$Br_Peak_LVL)
delta_1.8

#Bar-Vest
delta_1.9<-cliff.delta(d = VestCondition$Br_Peak_LVL,
                       f = ControlCondition$Br_Peak_LVL)
delta_1.9

#Bar-Vest>Nylon
delta_1.10<-cliff.delta(d = VestCondition$Br_Peak_LVL,
                        f = NylonCondition$Br_Peak_LVL)
delta_1.10

#Max-Vest
delta_1.11<-cliff.delta(d = VestCondition$max_Peak_LVL,
                        f = ControlCondition$max_Peak_LVL)
delta_1.11

#Max-Vest>Nylon
delta_1.12<-cliff.delta(d = VestCondition$max_Peak_LVL,
                        f = NylonCondition$max_Peak_LVL)
delta_1.12
#---------------------------------------------------------------------------------
#------------------RRF------------------------------------------------------------
#RMS-Leather
delta_2.1<-cliff.delta(d = LeatherCondition$NL_RMS_RVL,
                       f = ControlCondition$NL_RMS_RVL)
delta_2.1

#RMS-Nylon
delta_2.2<-cliff.delta(d = NylonCondition$NL_RMS_RVL,
                       f = ControlCondition$NL_RMS_RVL)
delta_2.2

#RMS-Vest
delta_2.3<-cliff.delta(d = VestCondition$NL_RMS_RVL,
                       f = ControlCondition$NL_RMS_RVL)
delta_2.3

#Mean-Leather
delta_2.4<-cliff.delta(d = LeatherCondition$NL_Mean_RVL,
                       f = ControlCondition$NL_Mean_RVL)
delta_2.4

#Mean-Nylon
delta_2.5<-cliff.delta(d = NylonCondition$NL_Mean_RVL,
                       f = ControlCondition$NL_Mean_RVL)
delta_2.5

#Mean-Vest
delta_2.6<-cliff.delta(d = VestCondition$NL_Mean_RVL,
                       f = ControlCondition$NL_Mean_RVL)
delta_2.6

#Bar-Leather
delta_2.7<-cliff.delta(d = LeatherCondition$Br_Peak_RVL,
                       f = ControlCondition$Br_Peak_RVL)
delta_2.7

#Bar-Nylon
delta_2.8<-cliff.delta(d = NylonCondition$Br_Peak_RVL,
                       f = ControlCondition$Br_Peak_RVL)
delta_2.8

#Bar-Vest
delta_2.9<-cliff.delta(d = VestCondition$Br_Peak_RVL,
                       f = ControlCondition$Br_Peak_RVL)
delta_2.9

#Max-Leather
delta_2.10<-cliff.delta(d = LeatherCondition$max_Peak_RVL,
                        f = ControlCondition$max_Peak_RVL)
delta_2.10

#Max-Vest
delta_2.11<-cliff.delta(d = VestCondition$max_Peak_RVL,
                        f = ControlCondition$max_Peak_RVL)
delta_2.11
#----------------------------------------------------------------------------
#----------------------------------LBF--------------------------------------- 
#RMS-Vest
delta_3.1<-cliff.delta(d = VestCondition$NL_RMS_LBF,
                       f = ControlCondition$NL_RMS_LBF)
delta_3.1

#RMS-Vest>Nylon
delta_3.2<-cliff.delta(d = VestCondition$NL_RMS_LBF,
                       f = NylonCondition$NL_RMS_LBF)
delta_3.2

#Mean-Vest
delta_3.3<-cliff.delta(d = VestCondition$NL_Mean_LBF,
                       f = ControlCondition$NL_Mean_LBF)
delta_3.3

#Mean-Vest>Nylon
delta_3.4<-cliff.delta(d = VestCondition$NL_Mean_LBF,
                       f = NylonCondition$NL_Mean_LBF)
delta_3.4
#--------------------------------------------------------------------------------
#------------------LRA------------------------------------------------------------
#RMS-Leather
delta_4.1<-cliff.delta(d = LeatherCondition$NL_RMS_LRA,
                       f = ControlCondition$NL_RMS_LRA)
delta_4.1

#RMS-Nylon
delta_4.2<-cliff.delta(d = NylonCondition$NL_RMS_LRA,
                       f = ControlCondition$NL_RMS_LRA)
delta_4.2

#RMS-Vest
delta_4.3<-cliff.delta(d = VestCondition$NL_RMS_LRA,
                       f = ControlCondition$NL_RMS_LRA)
delta_4.3

#Mean-Leather
delta_4.4<-cliff.delta(d = LeatherCondition$NL_Mean_LRA,
                       f = ControlCondition$NL_Mean_LRA)
delta_4.4

#Mean-Nylon
delta_4.5<-cliff.delta(d = NylonCondition$NL_Mean_LRA,
                       f = ControlCondition$NL_Mean_LRA)
delta_4.5

#Mean-Vest
delta_4.6<-cliff.delta(d = VestCondition$NL_Mean_LRA,
                       f = ControlCondition$NL_Mean_LRA)
delta_4.6
#----------------------------------------------------------------------------------
#---------------------------------RMT----------------------------------------------
#RMS-Vest
delta_5.1<-cliff.delta(d = VestCondition$NL_RMS_RMT,
                       f = ControlCondition$NL_RMS_RMT)
delta_5.1

#RMS-Vest>Leather
delta_5.2<-cliff.delta(d = VestCondition$NL_RMS_RMT,
                       f = LeatherCondition$NL_RMS_RMT)
delta_5.2

#Mean-Vest
delta_5.3<-cliff.delta(d = VestCondition$NL_Mean_RMT,
                       f = ControlCondition$NL_Mean_RMT)
delta_5.3

#Mean-Vest>Leather
delta_5.4<-cliff.delta(d = VestCondition$NL_Mean_RMT,
                       f = LeatherCondition$NL_Mean_RMT)
delta_5.4

#Mean-Vest>Nylon
delta_5.5<-cliff.delta(d = VestCondition$NL_Mean_RMT,
                       f = NylonCondition$NL_Mean_RMT)
delta_5.5

#Peak-Vest
delta_5.6<-cliff.delta(d = VestCondition$NL_Peak_RMT,
                       f = ControlCondition$NL_Peak_RMT)
delta_5.6
#----------------------------------------------------------------------------------
#---------------------------------LMT----------------------------------------------
#RMS-Vest
delta_6.1<-cliff.delta(d = VestCondition$NL_RMS_LMT,
                       f = ControlCondition$NL_RMS_LMT)
delta_6.1

#Mean-Vest
delta_6.2<-cliff.delta(d = VestCondition$NL_Mean_LMT,
                       f = ControlCondition$NL_Mean_LMT)
delta_6.2

#Peak-Leather
delta_6.3<-cliff.delta(d = LeatherCondition$NL_Peak_LMT,
                       f = ControlCondition$NL_Peak_LMT)
delta_6.3

#Peak-Vest
delta_6.4<-cliff.delta(d = VestCondition$NL_Peak_LMT,
                       f = ControlCondition$NL_Peak_LMT)
delta_6.4
#----------------------------------------------------------------------------------
#------------------------------------vGRF------------------------------------------
#RemoveSubject-4
ControlCondition_1<- rbind(ControlTwo, ControlThree, ControlFour, ControlFive, ControlSix, 
                           ControlSeven, ControlEight, ControlNine, ControlTen, ControlEleven,ControlTwelve,
                           ControlThirteen, ControlFourteen, ControlFifteen, ControlSixteen, ControlSeventeen,
                           ControlEighteen, ControlNineteen, ControlTwenty, ControlTwentyOne, ControlTwentyTwo,
                           ControlTwentyThree, ControlTwentyFour,ControlTwentyFive)

LeatherCondition_1<- rbind(LeatherTwo, LeatherThree, LeatherFour, LeatherFive, LeatherSix, 
                           LeatherSeven, LeatherEight, LeatherNine, LeatherTen, LeatherEleven,LeatherTwelve,
                           LeatherThirteen, LeatherFourteen, LeatherFifteen, LeatherSixteen, LeatherSeventeen,
                           LeatherEighteen, LeatherNineteen, LeatherTwenty, LeatherTwentyOne, LeatherTwentyTwo,
                           LeatherTwentyThree, LeatherTwentyFour, LeatherTwentyFive)

NylonCondition_1<- rbind(NylonTwo, NylonThree, NylonFour, NylonFive, NylonSix, 
                         NylonSeven, NylonEight, NylonNine, NylonTen, NylonEleven,NylonTwelve,
                         NylonThirteen, NylonFourteen, NylonFifteen, NylonSixteen, NylonSeventeen,
                         NylonEighteen, NylonNineteen, NylonTwenty, NylonTwentyOne, NylonTwentyTwo,
                         NylonTwentyThree, NylonTwentyFour, NylonTwentyFive)

VestCondition_1<- rbind(VestTwo, VestThree, VestFour, VestFive, VestSix, 
                        VestSeven, VestEight, VestNine, VestTen, VestEleven, VestTwelve,
                        VestThirteen, VestFourteen, VestFifteen, VestSixteen, VestSeventeen,
                        VestEighteen, VestNineteen, VestTwenty, VestTwentyOne, VestTwentyTwo,
                        VestTwentyThree, VestTwentyFour, VestTwentyFive)
#-------------------------------------------------------------------------------------------------------------
#Peak-vGRF-Vest-Left
delta_7.1<-cliff.delta(d = VestCondition_1$PeakForce_L_N,
                       f = ControlCondition_1$PeakForce_L_N)
delta_7.1

#Peak-vGRF-Vest-Right
delta_7.2<-cliff.delta(d = VestCondition_1$PeakForce_R_N,
                       f = ControlCondition_1$PeakForce_R_N)
delta_7.2

#Peak-vGRF-Vest>Leather-Right
delta_7.3<-cliff.delta(d = VestCondition_1$PeakForce_R_N,
                       f = LeatherCondition_1$PeakForce_R_N)
delta_7.3

#Mean-vGRF-Vest-Right
delta_7.4<-cliff.delta(d = VestCondition_1$meanForce_R_N,
                       f = ControlCondition_1$meanForce_R_N)
delta_7.4

#MeanAsymm-vGRF-Leather
delta_7.5<-cliff.delta(d = LeatherCondition_1$meanForceAsymm_percent,
                       f = ControlCondition_1$meanForceAsymm_percent)
delta_7.5

#MeanAsymm-vGRF-Nylon
delta_7.6<-cliff.delta(d = NylonCondition_1$meanForceAsymm_percent,
                       f = ControlCondition_1$meanForceAsymm_percent)
delta_7.6

#MeanAsymm-vGRF-Vest
delta_7.7<-cliff.delta(d = VestCondition_1$meanForceAsymm_percent,
                       f = ControlCondition_1$meanForceAsymm_percent)
delta_7.7

#MeanNorm-Left-vGRF-Leather
delta_7.8<-cliff.delta(d = LeatherCondition_1$meanForceNorm_L,
                       f = ControlCondition_1$meanForceNorm_L)
delta_7.8

#MeanNorm-Left-vGRF-Nylon
delta_7.9<-cliff.delta(d = NylonCondition_1$meanForceNorm_L,
                       f = ControlCondition_1$meanForceNorm_L)
delta_7.9

#MeanNorm-Left-vGRF-Vest
delta_7.10<-cliff.delta(d = VestCondition_1$meanForceNorm_L,
                        f = ControlCondition_1$meanForceNorm_L)
delta_7.10
#-------------------------------------------------------------------------------
#----------------------Comfort-Data---------------------------------------------
#Comfort- 4=Most Comfortable, 1=Least, Restrictive- 4=Most Restrictive, 1=Least

Comfort_Data<-cbind(hdata$Comfort, hdata$Comfort_Rank, hdata$Restrictive, hdata$Restrictive_Rank)

hdata$Condition <- factor(hdata$Condition,levels = c("Control", "Leather belt","Nylon Belt","Vest"),labels = c(1, 2, 3, 4)) 

kruskal.test(Comfort ~ Comfort_Rank, data = hdata)
dunn_test(hdata, Comfort ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)

kruskal.test(Restrictive ~ Rstrictive_Rank, data = hdata)
dunn_test(hdata, Restrictive ~ Rstrictive_Rank, p.adjust.method = "none", detailed = FALSE)

cor(x = hdata$Comfort_Rank, y = as.numeric(hdata$Condition), method = c("spearman"))

cor(x = hdata$Comfort_Rank, y = hdata$Comfort, method = c("spearman"))

cor(x = hdata$Rstrictive_Rank, y = hdata$Restrictive, method = c("spearman"))

cor(x = hdata$Comfort_Rank, y = hdata$Rstrictive_Rank, method = c("spearman"))

cor(x = hdata$Comfort, y = hdata$Restrictive, method = c("spearman"))

cor(hdata$Condition, hdata$Comfort_Rank)

hdata$Condition <- factor(hdata$Condition,levels = c(1,2,3,4),labels = c("Control", "Leather belt","Nylon Belt","Vest")) 
#---------------LRF--------------------------------------------------------------------
Model12<-nonpartest(L_RF ~ Comfort_Rank, data = hdata ,permreps=1000)
Model12
#Sig
kruskal.test(NL_RMS_LVL ~ Comfort_Rank, data = hdata)
dunn_test(hdata, NL_RMS_LVL ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Mean_LVL ~ Comfort_Rank, data = hdata)
dunn_test(hdata, NL_Mean_LVL ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Peak_LVL ~ Comfort_Rank, data = hdata)
#Not Sig

kruskal.test(Br_Peak_LVL ~ Comfort_Rank, data = hdata)
dunn_test(hdata, Br_Peak_LVL ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)

kruskal.test(max_Peak_LVL ~ Comfort_Rank, data = hdata)
dunn_test(hdata, max_Peak_LVL ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)
#------------------------------------------------------------------------------------------

#---------------RRF--------------------------------------------------------------------
Model13<-nonpartest(R_RF ~ Comfort_Rank, data = hdata ,permreps=1000)
Model13
#Sig
kruskal.test(NL_RMS_RVL ~ Comfort_Rank, data = hdata)
dunn_test(hdata, NL_RMS_RVL ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Mean_RVL ~ Comfort_Rank, data = hdata)
dunn_test(hdata, NL_Mean_RVL ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Peak_RVL ~ Comfort_Rank, data = hdata)
#Not Sig

kruskal.test(Br_Peak_RVL ~ Comfort_Rank, data = hdata)
dunn_test(hdata, Br_Peak_RVL ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)

kruskal.test(max_Peak_RVL ~ Comfort_Rank, data = hdata)
dunn_test(hdata, max_Peak_RVL ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)
#------------------------------------------------------------------------------------------

#---------------LBF--------------------------------------------------------------------
Model14<-nonpartest(L_BF ~ Comfort_Rank, data = hdata ,permreps=1000)
Model14
#Sig
kruskal.test(NL_RMS_LBF ~ Comfort_Rank, data = hdata)
dunn_test(hdata, NL_RMS_LBF ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Mean_LBF ~ Comfort_Rank, data = hdata)
dunn_test(hdata, NL_Mean_LBF ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Peak_LBF ~ Comfort_Rank, data = hdata)
#Not Sig

kruskal.test(Br_Peak_LBF ~ Comfort_Rank, data = hdata)
#Not Sig

kruskal.test(max_Peak_LBF ~ Comfort_Rank, data = hdata)
#Not Sig
#------------------------------------------------------------------------------------------

#---------------RBF--------------------------------------------------------------------
Model15<-nonpartest(R_BF ~ Comfort_Rank, data = hdata ,permreps=1000)
Model15
#------------------------------------------------------------------------------------------

#---------------LRA--------------------------------------------------------------------
Model16<-nonpartest(L_RA ~ Comfort_Rank, data = hdata ,permreps=1000)
Model16
#Sig
kruskal.test(NL_RMS_LRA ~ Comfort_Rank, data = hdata)
dunn_test(hdata, NL_RMS_LRA ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Mean_LRA ~ Comfort_Rank, data = hdata)
dunn_test(hdata, NL_Mean_LRA ~ Comfort_Rank, p.adjust.method = "none", detailed = FALSE)

kruskal.test(NL_Peak_LRA ~ Comfort_Rank, data = hdata)
#Not Sig

kruskal.test(Br_Peak_LRA ~ Comfort_Rank, data = hdata)
#Not Sig

kruskal.test(max_Peak_LRA ~ Comfort_Rank, data = hdata)
#Not Sig
#------------------------------------------------------------------------------------------

#---------------RRA--------------------------------------------------------------------
Model17<-nonpartest(R_RA ~ Comfort_Rank, data = hdata ,permreps=1000)
Model17
#Not Sig
#------------------------------------------------------------------------------------------

#---------------LMT--------------------------------------------------------------------
Model18<-nonpartest(L_MT ~ Comfort_Rank, data = hdata ,permreps=1000)
Model18
#Not Sig
#------------------------------------------------------------------------------------------

#---------------RMT--------------------------------------------------------------------
Model19<-nonpartest(L_MT ~ Comfort_Rank, data = hdata ,permreps=1000)
Model19
#Not Sig
#------------------------------------------------------------------------------------------

Model20<-nonpartest(Hinge_GRF ~ Comfort_Rank, data = hdata2 ,permreps=1000)
Model20

#_____________________________________________________________________________________________
#______________________________vvv__________Do Last________________________
#Graphs of data
#Catigorical 
# create a dataset
#BW_Hinge_RMS_Data
Muscles <- c(rep("L_RF" , 4) , rep("R_RF" , 4) , rep("L_BF" , 4) , rep("R_BF" , 4),  rep("L_RA" , 4) , rep("R_RA" , 4) , rep("L_MT" , 4) , rep("R_MT" , 4))
cond <- rep(c("Control", "Leather" , "Nylon" , "Vest") , 4)
values <- (Hinge_RMS)
data <- data.frame(Muscles,cond,values)

hdata$Condition <- factor(hdata$Condition,levels = c("Control", "Leather belt","Nylon Belt","Vest"),labels = c(1, 2, 3, 4)) 
cor(as.matrix(hdata))

# Grouped
ggplot(hdata, aes(fill=, y=values,  x=Muscles)) + 
  geom_bar(position="dodge", stat="identity")



#Violin
ggplot(hdata, aes(x=Condition, y=NL_RMS_RMT)) + 
  geom_violin(trim=FALSE, fill="yellow")+
  labs(title="Right Multifidus",x="Condition", y = "EMG (mV)")+
  geom_boxplot(width=0.1)+
  theme_classic()                 


