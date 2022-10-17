#Belt Study Hinge Paper
#written by:  James Kearney & Dr. Joel Martin
#October 2022

#Section outline

#1 - load libraries
#2 - load file
#3 - label variables
#4- Explore initial data, Remove Outliers
#5 - Separate Conditions
#6- Remove Extreme Variables (Winzorize)
#7- Re-connect data
#8 - Explore data 
#9 - Run Statistics

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


#-----------------------3 - Label variables -------------------------
## Change to Factor
hdata$Subject<-as.factor(hdata$Subject) 
hdata$Condition<-as.factor(hdata$Condition) 

#CHECK THE DATA SET STRUCTURE
str(hdata)

#LABEL IV
hdata$Condition <- factor(hdata$Condition,levels = c(1,2,3,4),labels = c("Control", "Leather belt","Nylon Belt","Vest")) 

##Label BMI
#BMI to Categories (3 = Yellow = 25-29.9, 2 = Green = 18 - 24.9, 1 = Blue < 17.9)
hdata$BMI<-as.factor(hdata$BMI)
hdata$BMI <- factor(hdata$BMI,levels = c(2,3),labels = c("Green", "Yellow"))

hdata2 <- hdata[-c(1, 2, 3, 4), ]

#--------------------------Explore - And - Remove - Outliers ------------
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
boxplot(peakForceAsymm_percent~Condition,data=hdata2, main="Peak Asymm",
        xlab="Percentage", ylab="Condition")$out

boxplot(meanForceAsymm_percent~Condition,data=hdata2, main="Mean Asymm",
        xlab="Percentage", ylab="Condition")$out


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
shapiro.test(hdata2$peakForceAsymm_percent)
shapiro.test(hdata2$meanForceAsymm_percent)

# Remove Outliers
hdata <- hdata[-c(13, 14, 15, 16, 45, 46, 47, 48, 81, 82, 83, 84), ]
hdata2 <- hdata2[-c(9, 10, 11, 12, 41, 42, 43, 44, 77, 78, 79, 80), ]


# Make a Copy
HD = hdata
HD2 = hdata2

#-----------------------5 - Separate Conditions -------------------------
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

#-----------------------6 - Winzorize Data -------------------------
#Winzor for each variable (VestCondition$NL_RMS_LVL), Label it into a variable, put the 4 variables back into a single variable then add them back into a data set
#RMS
C_RMS_LVL<-winsorize(ControlCondition$NL_RMS_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                            na.rm = FALSE, type = 7)
L_RMS_LVL<-winsorize(LeatherCondition$NL_RMS_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
N_RMS_LVL<-winsorize(NylonCondition$NL_RMS_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
V_RMS_LVL<-winsorize(VestCondition$NL_RMS_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)

#--------------------------------------------------------------------
C_RMS_RVL<-winsorize(ControlCondition$NL_RMS_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                            na.rm = FALSE, type = 7)
L_RMS_RVL<-winsorize(LeatherCondition$NL_RMS_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
N_RMS_RVL<-winsorize(NylonCondition$NL_RMS_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
V_RMS_RVL<-winsorize(VestCondition$NL_RMS_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)

#--------------------------------------------------------------------
C_RMS_LBF<-winsorize(ControlCondition$NL_RMS_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                            na.rm = FALSE, type = 7)
L_RMS_LBF<-winsorize(LeatherCondition$NL_RMS_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
N_RMS_LBF<-winsorize(NylonCondition$NL_RMS_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
V_RMS_LBF<-winsorize(VestCondition$NL_RMS_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_RMS_RBF<-winsorize(ControlCondition$NL_RMS_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                            na.rm = FALSE, type = 7)
L_RMS_RBF<-winsorize(LeatherCondition$NL_RMS_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
N_RMS_RBF<-winsorize(NylonCondition$NL_RMS_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
V_RMS_RBF<-winsorize(VestCondition$NL_RMS_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_RMS_LRA<-winsorize(ControlCondition$NL_RMS_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                            na.rm = FALSE, type = 7)
L_RMS_LRA<-winsorize(LeatherCondition$NL_RMS_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
N_RMS_LRA<-winsorize(NylonCondition$NL_RMS_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
V_RMS_LRA<-winsorize(VestCondition$NL_RMS_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_RMS_RRA<-winsorize(ControlCondition$NL_RMS_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                            na.rm = FALSE, type = 7)
L_RMS_RRA<-winsorize(LeatherCondition$NL_RMS_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
N_RMS_RRA<-winsorize(NylonCondition$NL_RMS_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
V_RMS_RRA<-winsorize(VestCondition$NL_RMS_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_RMS_LMT<-winsorize(ControlCondition$NL_RMS_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                            na.rm = FALSE, type = 7)
L_RMS_LMT<-winsorize(LeatherCondition$NL_RMS_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
N_RMS_LMT<-winsorize(NylonCondition$NL_RMS_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
V_RMS_LMT<-winsorize(VestCondition$NL_RMS_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_RMS_RMT<-winsorize(ControlCondition$NL_RMS_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                            na.rm = FALSE, type = 7)
L_RMS_RMT<-winsorize(LeatherCondition$NL_RMS_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
N_RMS_RMT<-winsorize(NylonCondition$NL_RMS_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)
V_RMS_RMT<-winsorize(VestCondition$NL_RMS_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                     na.rm = FALSE, type = 7)

#------------------------ Peak - Bar --------------------------------
C_Br_LVL<-winsorize(ControlCondition$Br_Peak_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                             na.rm = FALSE, type = 7)
L_Br_LVL<-winsorize(LeatherCondition$Br_Peak_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_Br_LVL<-winsorize(NylonCondition$Br_Peak_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_Br_LVL<-winsorize(VestCondition$Br_Peak_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#---------------------------------------------------------------------
C_Br_RVL<-winsorize(ControlCondition$Br_Peak_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                             na.rm = FALSE, type = 7)
L_Br_RVL<-winsorize(LeatherCondition$Br_Peak_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_Br_RVL<-winsorize(NylonCondition$Br_Peak_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_Br_RVL<-winsorize(VestCondition$Br_Peak_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_Br_LBF<-winsorize(ControlCondition$Br_Peak_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                             na.rm = FALSE, type = 7)
L_Br_LBF<-winsorize(LeatherCondition$Br_Peak_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_Br_LBF<-winsorize(NylonCondition$Br_Peak_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_Br_LBF<-winsorize(VestCondition$Br_Peak_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_Br_RBF<-winsorize(ControlCondition$Br_Peak_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                             na.rm = FALSE, type = 7)
L_Br_RBF<-winsorize(LeatherCondition$Br_Peak_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_Br_RBF<-winsorize(NylonCondition$Br_Peak_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_Br_RBF<-winsorize(VestCondition$Br_Peak_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_Br_LRA<-winsorize(ControlCondition$Br_Peak_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                             na.rm = FALSE, type = 7)
L_Br_LRA<-winsorize(LeatherCondition$Br_Peak_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_Br_LRA<-winsorize(NylonCondition$Br_Peak_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_Br_LRA<-winsorize(VestCondition$Br_Peak_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_Br_RRA<-winsorize(ControlCondition$Br_Peak_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                             na.rm = FALSE, type = 7)
L_Br_RRA<-winsorize(LeatherCondition$Br_Peak_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_Br_RRA<-winsorize(NylonCondition$Br_Peak_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_Br_RRA<-winsorize(VestCondition$Br_Peak_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#-----------------------------------------------------------------------
C_Br_LMT<-winsorize(ControlCondition$Br_Peak_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                             na.rm = FALSE, type = 7)
L_Br_LMT<-winsorize(LeatherCondition$Br_Peak_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_Br_LMT<-winsorize(NylonCondition$Br_Peak_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_Br_LMT<-winsorize(VestCondition$Br_Peak_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#-----------------------------------------------------------------------
C_Br_RMT<-winsorize(ControlCondition$Br_Peak_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                             na.rm = FALSE, type = 7)
L_Br_RMT<-winsorize(LeatherCondition$Br_Peak_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_Br_RMT<-winsorize(NylonCondition$Br_Peak_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_Br_RMT<-winsorize(VestCondition$Br_Peak_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#------------------------ Peak - MAX --------------------------------
C_max_LVL<-winsorize(ControlCondition$max_Peak_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
L_max_LVL<-winsorize(LeatherCondition$max_Peak_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_max_LVL<-winsorize(NylonCondition$max_Peak_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_max_LVL<-winsorize(VestCondition$max_Peak_LVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#---------------------------------------------------------------------
C_max_RVL<-winsorize(ControlCondition$max_Peak_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
L_max_RVL<-winsorize(LeatherCondition$max_Peak_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_max_RVL<-winsorize(NylonCondition$max_Peak_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_max_RVL<-winsorize(VestCondition$max_Peak_RVL, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_max_LBF<-winsorize(ControlCondition$max_Peak_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
L_max_LBF<-winsorize(LeatherCondition$max_Peak_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_max_LBF<-winsorize(NylonCondition$max_Peak_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_max_LBF<-winsorize(VestCondition$max_Peak_LBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_max_RBF<-winsorize(ControlCondition$max_Peak_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
L_max_RBF<-winsorize(LeatherCondition$max_Peak_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_max_RBF<-winsorize(NylonCondition$max_Peak_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_max_RBF<-winsorize(VestCondition$max_Peak_RBF, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_max_LRA<-winsorize(ControlCondition$max_Peak_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
L_max_LRA<-winsorize(LeatherCondition$max_Peak_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_max_LRA<-winsorize(NylonCondition$max_Peak_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_max_LRA<-winsorize(VestCondition$max_Peak_LRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#----------------------------------------------------------------------
C_max_RRA<-winsorize(ControlCondition$max_Peak_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
L_max_RRA<-winsorize(LeatherCondition$max_Peak_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_max_RRA<-winsorize(NylonCondition$max_Peak_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_max_RRA<-winsorize(VestCondition$max_Peak_RRA, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#-----------------------------------------------------------------------
C_max_LMT<-winsorize(ControlCondition$max_Peak_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
L_max_LMT<-winsorize(LeatherCondition$max_Peak_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_max_LMT<-winsorize(NylonCondition$max_Peak_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_max_LMT<-winsorize(VestCondition$max_Peak_LMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)

#-----------------------------------------------------------------------
C_max_RMT<-winsorize(ControlCondition$max_Peak_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
L_max_RMT<-winsorize(LeatherCondition$max_Peak_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
N_max_RMT<-winsorize(NylonCondition$max_Peak_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
V_max_RMT<-winsorize(VestCondition$max_Peak_RMT, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                    na.rm = FALSE, type = 7)
#Remove Subj. 4
ControlCondition2 <- ControlCondition[-c(1), ]
LeatherCondition2 <- LeatherCondition[-c(1), ]
NylonCondition2 <- NylonCondition[-c(1), ]
VestCondition2 <- VestCondition[-c(1), ]

#GRF
ControlCondition2$peakForceAsymm_percent<-as.numeric(ControlCondition2$peakForceAsymm_percent)
C_Peak_Asymm<-winsorize(ControlCondition2$peakForceAsymm_percent, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                                         na.rm = FALSE, type = 7)

LeatherCondition2$peakForceAsymm_percent<-as.numeric(LeatherCondition2$peakForceAsymm_percent)
L_Peak_Asymm<-winsorize(LeatherCondition2$peakForceAsymm_percent, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                        na.rm = FALSE, type = 7)

NylonCondition2$peakForceAsymm_percent<-as.numeric(NylonCondition2$peakForceAsymm_percent)
N_Peak_Asymm<-winsorize(NylonCondition2$peakForceAsymm_percent, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                        na.rm = FALSE, type = 7)

VestCondition2$peakForceAsymm_percent<-as.numeric(VestCondition2$peakForceAsymm_percent)
V_Peak_Asymm<-winsorize(VestCondition2$peakForceAsymm_percent, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                        na.rm = FALSE, type = 7)

#----------------------------------------------------------------------------
ControlCondition2$meanForceAsymm_percent<-as.numeric(ControlCondition2$meanForceAsymm_percent)
C_Mean_Asymm<-winsorize(ControlCondition2$meanForceAsymm_percent, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                                         na.rm = FALSE, type = 7)

LeatherCondition2$meanForceAsymm_percent<-as.numeric(LeatherCondition2$meanForceAsymm_percent)
L_Mean_Asymm<-winsorize(LeatherCondition2$meanForceAsymm_percent, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                        na.rm = FALSE, type = 7)

NylonCondition2$meanForceAsymm_percent<-as.numeric(NylonCondition2$meanForceAsymm_percent)
N_Mean_Asymm<-winsorize(NylonCondition2$meanForceAsymm_percent, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                        na.rm = FALSE, type = 7)

VestCondition2$meanForceAsymm_percent<-as.numeric(VestCondition2$meanForceAsymm_percent)
V_Mean_Asymm<-winsorize(VestCondition2$meanForceAsymm_percent, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
                        na.rm = FALSE, type = 7)

#----------------------------7- Re-Connect Data ---------------------------
#RMS
HD$NL_RMS_LVL <- c(C_RMS_LVL,  L_RMS_LVL,  N_RMS_LVL,  V_RMS_LVL)
HD$NL_RMS_RVL <- c(C_RMS_RVL,  L_RMS_RVL,  N_RMS_RVL,  V_RMS_RVL)
HD$NL_RMS_LBF <- c(C_RMS_LBF,  L_RMS_LBF,  N_RMS_LBF,  V_RMS_LBF)
HD$NL_RMS_RBF <- c(C_RMS_RBF,  L_RMS_RBF,  N_RMS_RBF,  V_RMS_RBF)
HD$NL_RMS_LRA <- c(C_RMS_LRA,  L_RMS_LRA,  N_RMS_LRA,  V_RMS_LRA)
HD$NL_RMS_RRA <- c(C_RMS_RRA,  L_RMS_RRA,  N_RMS_RRA,  V_RMS_RRA)
HD$NL_RMS_LMT <- c(C_RMS_LMT,  L_RMS_LMT,  N_RMS_LMT,  V_RMS_LMT)
HD$NL_RMS_RMT <- c(C_RMS_RMT,  L_RMS_RMT,  N_RMS_RMT,  V_RMS_RMT)

#Bar
HD$Br_Peak_LVL <- c(C_Br_LVL,  L_Br_LVL,  N_Br_LVL,  V_Br_LVL)
HD$Br_Peak_RVL <- c(C_Br_RVL,  L_Br_RVL,  N_Br_RVL,  V_Br_RVL)
HD$Br_Peak_LBF <- c(C_Br_LBF,  L_Br_LBF,  N_Br_LBF,  V_Br_LBF)
HD$Br_Peak_RBF <- c(C_Br_RBF,  L_Br_RBF,  N_Br_RBF,  V_Br_RBF)
HD$Br_Peak_LRA <- c(C_Br_LRA,  L_Br_LRA,  N_Br_LRA,  V_Br_LRA)
HD$Br_Peak_RRA <- c(C_Br_RRA,  L_Br_RRA,  N_Br_RRA,  V_Br_RRA)
HD$Br_Peak_LMT <- c(C_Br_LMT,  L_Br_LMT,  N_Br_LMT,  V_Br_LMT)
HD$Br_Peak_RMT <- c(C_Br_RMT,  L_Br_RMT,  N_Br_RMT,  V_Br_RMT)

#Max
HD$max_Peak_LVL <- c(C_max_LVL,  L_max_LVL,  N_max_LVL,  V_max_LVL)
HD$max_Peak_RVL <- c(C_max_RVL,  L_max_RVL,  N_max_RVL,  V_max_RVL)
HD$max_Peak_LBF <- c(C_max_LBF,  L_max_LBF,  N_max_LBF,  V_max_LBF)
HD$max_Peak_RBF <- c(C_max_RBF,  L_max_RBF,  N_max_RBF,  V_max_RBF)
HD$max_Peak_LRA <- c(C_max_LRA,  L_max_LRA,  N_max_LRA,  V_max_LRA)
HD$max_Peak_RRA <- c(C_max_RRA,  L_max_RRA,  N_max_RRA,  V_max_RRA)
HD$max_Peak_LMT <- c(C_max_LMT,  L_max_LMT,  N_max_LMT,  V_max_LMT)
HD$max_Peak_RMT <- c(C_max_RMT,  L_max_RMT,  N_max_RMT,  V_max_RMT)

#vGRF
HD2$peakForceAsymm_percent <- c(C_Peak_Asymm, L_Peak_Asymm, N_Peak_Asymm, V_Peak_Asymm)
HD2$meanForceAsymm_percent <- c(C_Mean_Asymm, L_Mean_Asymm, N_Mean_Asymm, V_Mean_Asymm)

#-------------------------------8 - Re-Explore ------------------------------
HD2$peakForceAsymm_percent<-as.numeric(HD2$peakForceAsymm_percent)
HD2$meanForceAsymm_percent<-as.numeric(HD2$meanForceAsymm_percent)

#-------------------------------------RMS
boxplot(NL_RMS_LVL~Condition,data=HD, main="LVL RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out 

boxplot(NL_RMS_RVL~Condition,data=HD, main="RVL RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_LBF~Condition,data=HD, main="LBF RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_RBF~Condition,data=HD, main="RBF RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_LRA~Condition,data=HD, main="LRA RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_RRA~Condition,data=HD, main="RRA RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_LMT~Condition,data=HD, main="LMT RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

boxplot(NL_RMS_RMT~Condition,data=HD, main="RMT RMS",
        xlab="EMG Mean (mV)", ylab="Condition")$out

#------------------------------Bar Peak-------------------------------------
boxplot(Br_Peak_LVL~Condition,data=HD, main="LVL Peak",
        xlab="EMG Peak (mV)", ylab="Condition") 

boxplot(Br_Peak_RVL~Condition,data=HD, main="RVL Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_LBF~Condition,data=HD, main="LBF Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_RBF~Condition,data=HD, main="RBF Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_LRA~Condition,data=HD, main="LRA Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_RRA~Condition,data=HD, main="RRA Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_LMT~Condition,data=HD, main="LMT Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(Br_Peak_RMT~Condition,data=HD, main="RMT Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

#------------------------------Max Peak-------------------------------------
boxplot(max_Peak_LVL~Condition,data=HD, main="LVL Peak",
        xlab="EMG Peak (mV)", ylab="Condition") 

boxplot(max_Peak_RVL~Condition,data=HD, main="RVL Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_LBF~Condition,data=HD, main="LBF Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_RBF~Condition,data=HD, main="RBF Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_LRA~Condition,data=HD, main="LRA Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_RRA~Condition,data=HD, main="RRA Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_LMT~Condition,data=HD, main="LMT Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

boxplot(max_Peak_RMT~Condition,data=HD, main="RMT Peak",
        xlab="EMG Peak (mV)", ylab="Condition")

#-------------------------------vGRF---------------------------------
boxplot(peakForceAsymm_percent~Condition,data=HD2, main="Peak Asymm",
        xlab="Percentage", ylab="Condition")$out

boxplot(meanForceAsymm_percent~Condition,data=HD2, main="Mean Asymm",
        xlab="Percentage", ylab="Condition")$out

#Normality-----------------------------------
shapiro.test(HD$Age)
shapiro.test(HD$Sex)
shapiro.test(HD$Height)
shapiro.test(HD$Mass)
shapiro.test(HD$BMI_V)
shapiro.test(HD$Comfort)
shapiro.test(HD$Comfort_Rank)
shapiro.test(HD$Restrictive)
shapiro.test(HD$Rstrictive_Rank)

#----------RMS----------------------
shapiro.test(hdata$NL_RMS_LVL)
shapiro.test(hdata$NL_RMS_RVL)
shapiro.test(hdata$NL_RMS_LBF)
shapiro.test(hdata$NL_RMS_RBF)
shapiro.test(hdata$NL_RMS_LRA) 
shapiro.test(hdata$NL_RMS_RRA)
shapiro.test(hdata$NL_RMS_LMT)
shapiro.test(hdata$NL_RMS_RMT)

#---------Peak-Bar---------------------
shapiro.test(HD$Br_Peak_LVL)
shapiro.test(HD$Br_Peak_RVL)
shapiro.test(HD$Br_Peak_LBF)
shapiro.test(HD$Br_Peak_RBF)
shapiro.test(HD$Br_Peak_LRA) 
shapiro.test(HD$Br_Peak_RRA)
shapiro.test(HD$Br_Peak_LMT)#Not Sig
shapiro.test(HD$Br_Peak_RMT)

#---------Peak-Max---------------------
shapiro.test(HD$max_Peak_LVL)
shapiro.test(HD$max_Peak_RVL)
shapiro.test(HD$max_Peak_LBF)
shapiro.test(HD$max_Peak_RBF)
shapiro.test(HD$max_Peak_LRA) 
shapiro.test(HD$max_Peak_RRA) 
shapiro.test(HD$max_Peak_LMT)#Not Sig
shapiro.test(HD$max_Peak_RMT)

#-------------------------------------vGRF
shapiro.test(HD2$peakForceAsymm_percent)
shapiro.test(HD2$meanForceAsymm_percent)

#--------------------------------------------------------------------------
#Mean and standard deviation for each variable by condition factor

summaryBy(Comfort~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Comfort_Rank ~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Restrictive ~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Rstrictive_Rank ~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

#----------------RMS-BW Hinge---------------------------------------------
summaryBy(NL_RMS_LVL~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_RVL~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_LBF~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_RBF~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_LRA~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_RRA~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_LMT~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(NL_RMS_RMT~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

#---------------------------Bar-Peak---------------------------------
summaryBy(Br_Peak_LVL~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_RVL~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_LBF~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_RBF~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_LRA~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_RRA~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_LMT~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(Br_Peak_RMT~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

#------------------------Max-Peak---------------------------------
summaryBy(max_Peak_LVL~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_RVL~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_LBF~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_RBF~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_LRA~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_RRA~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_LMT~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(max_Peak_RMT~ Condition, data = HD,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

#----------------vGRF--------------------------------------
summaryBy(peakForceAsymm_percent ~ Condition, data = HD2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

summaryBy(meanForceAsymm_percent~ Condition, data = HD2,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

#------------------------------9 - Statistics -------------------------------
#Friedman's Test, Wilcoxon signed ranks test, Cliff's Delta
HD<-HD[order(HD$Subject),]

#Need to change them to factors
HD$Condition <- factor(HD$Condition)
HD$Subject <- factor(HD$Subject)
HD$NL_RMS_LVL <- factor(HD$NL_RMS_LVL)
HD$NL_RMS_RVL <- factor(HD$NL_RMS_RVL)
HD$NL_RMS_LBF <- factor(HD$NL_RMS_LBF)
HD$NL_RMS_RBF <- factor(HD$NL_RMS_RBF)
HD$NL_RMS_LRA <- factor(HD$NL_RMS_LRA)
HD$NL_RMS_RRA <- factor(HD$NL_RMS_RRA)
HD$NL_RMS_LMT <- factor(HD$NL_RMS_LMT)
HD$NL_RMS_RMT <- factor(HD$NL_RMS_RMT)

#RMS 
Model1.1 <- friedman_test(NL_RMS_LVL ~ Condition | Subject, data = HD)
Model1.1

Model1.2 <- friedman_test(NL_RMS_RVL ~ Condition | Subject, data = HD)
Model1.2

Model1.3 <- friedman_test(NL_RMS_LBF ~ Condition | Subject, data = HD)
Model1.3

Model1.4 <- friedman_test(NL_RMS_RBF ~ Condition | Subject, data = HD)
Model1.4

Model1.5 <- friedman_test(NL_RMS_LRA ~ Condition | Subject, data = HD)
Model1.5

Model1.6 <- friedman_test(NL_RMS_RRA ~ Condition | Subject, data = HD)
Model1.6

Model1.7 <- friedman_test(NL_RMS_LMT ~ Condition | Subject, data = HD)
Model1.7

Model1.8 <- friedman_test(NL_RMS_RMT ~ Condition | Subject, data = HD)
Model1.8

#Bar
HD$Br_Peak_LVL <- factor(HD$Br_Peak_LVL)
HD$Br_Peak_RVL <- factor(HD$Br_Peak_RVL)
HD$Br_Peak_LBF <- factor(HD$Br_Peak_LBF)
HD$Br_Peak_RBF <- factor(HD$Br_Peak_RBF)
HD$Br_Peak_LRA <- factor(HD$Br_Peak_LRA)
HD$Br_Peak_RRA <- factor(HD$Br_Peak_RRA)
HD$Br_Peak_LMT <- factor(HD$Br_Peak_LMT)
HD$Br_Peak_RMT <- factor(HD$Br_Peak_RMT)

Model2.1 <- friedman_test(Br_Peak_LVL ~ Condition | Subject, data = HD)
Model2.1

Model2.2 <- friedman_test(Br_Peak_RVL ~ Condition | Subject, data = HD)
Model2.2

Model2.3 <- friedman_test(Br_Peak_LBF ~ Condition | Subject, data = HD)
Model2.3

Model2.4 <- friedman_test(Br_Peak_RBF ~ Condition | Subject, data = HD)
Model2.4

Model2.5 <- friedman_test(Br_Peak_LRA ~ Condition | Subject, data = HD)
Model2.5

Model2.6 <- friedman_test(Br_Peak_RRA ~ Condition | Subject, data = HD)
Model2.6

Model2.7 <- friedman_test(Br_Peak_LMT ~ Condition | Subject, data = HD)
Model2.7

Model2.8 <- friedman_test(Br_Peak_RMT ~ Condition | Subject, data = HD)
Model2.8

#Max
HD$max_Peak_LVL <- factor(HD$max_Peak_LVL)
HD$max_Peak_RVL <- factor(HD$max_Peak_RVL)
HD$max_Peak_LBF <- factor(HD$max_Peak_LBF)
HD$max_Peak_RBF <- factor(HD$max_Peak_RBF)
HD$max_Peak_LRA <- factor(HD$max_Peak_LRA)
HD$max_Peak_RRA <- factor(HD$max_Peak_RRA)
HD$max_Peak_LMT <- factor(HD$max_Peak_LMT)
HD$max_Peak_RMT <- factor(HD$max_Peak_RMT)

Model3.1 <- friedman_test(max_Peak_LVL ~ Condition | Subject, data = HD)
Model3.1

Model3.2 <- friedman_test(max_Peak_RVL ~ Condition | Subject, data = HD)
Model3.2

Model3.3 <- friedman_test(max_Peak_LBF ~ Condition | Subject, data = HD)
Model3.3

Model3.4 <- friedman_test(max_Peak_RBF ~ Condition | Subject, data = HD)
Model3.4

Model3.5 <- friedman_test(max_Peak_LRA ~ Condition | Subject, data = HD)
Model3.5

Model3.6 <- friedman_test(max_Peak_RRA ~ Condition | Subject, data = HD)
Model3.6

Model3.7 <- friedman_test(max_Peak_LMT ~ Condition | Subject, data = HD)
Model3.7

Model3.8 <- friedman_test(max_Peak_RMT ~ Condition | Subject, data = HD)
Model3.8

#vGRF
HD2$Condition <- factor(HD2$Condition)
HD2$Subject <- factor(HD2$Subject)
HD2$peakForceAsymm_percent<-factor(HD2$peakForceAsymm_percent)
HD2$meanForceAsymm_percent<-factor(HD2$meanForceAsymm_percent)

Model4.1 <- friedman_test(peakForceAsymm_percent ~ Condition | Subject, data = HD2)
Model4.1

Model4.2 <- friedman_test(meanForceAsymm_percent ~ Condition | Subject, data = HD2)
Model4.2

#STOPPED HERE___________________________________________________
#Code for Cliff's Delta
delta_1.1<-cliff.delta(d = LeatherCondition$NL_RMS_LVL,
                       f = ControlCondition$NL_RMS_LVL)
delta_1.1

LRFposthoc <- pairwise.wilcox.test(LRF, condition, paired = TRUE, exact = FALSE,  p.adj = "bonferroni", correct = FALSE)
LRFposthoc

install.packages('coin')
library(coin)
#calculate effect size r
wilcox_effsize(df, LRF ~ condition, paired = TRUE)

#right rectus femoris 
res.fried.winsRRF <- friedman_test(RRF ~ condition | subject, data = df)
res.fried.winsRRF

RRFposthoc <- pairwise.wilcox.test(RRF, condition, paired = TRUE, exact = FALSE,  p.adj = "bonferroni", correct = FALSE)
RRFposthoc

wilcox_effsize(df, RRF ~ condition, paired = TRUE)

#left biceps femoris 
res.fried.winsLBF <- friedman_test(LBF ~ condition | subject, data = df)
res.fried.winsLBF

LBFposthoc <- pairwise.wilcox.test(LBF, condition, paired = TRUE, exact = FALSE,  p.adj = "bonferroni", correct = FALSE)
LBFposthoc

#right biceps femoris 
res.fried.winsRBF <- friedman_test(RBF ~ condition | subject, data = df)
res.fried.winsRBF

#left abdominals
res.fried.winsLAB <- friedman_test(LAB ~ condition | subject, data = df)
res.fried.winsLAB

#right abdominals
res.fried.winsRAB <- friedman_test(RAB ~ condition | subject, data = df)
res.fried.winsRAB

#left multifidus
res.fried.winsLMF <- friedman_test(LMF ~ condition | subject, data = df)
res.fried.winsLMF

LMFposthoc <- pairwise.wilcox.test(LMF, condition, paired = TRUE, exact = FALSE,  p.adj = "bonferroni", correct = FALSE)
LMFposthoc

wilcox_effsize(df, LMF ~ condition, paired = TRUE)

#right multifidus
res.fried.winsRMF <- friedman_test(RMF ~ condition | subject, data = df)
res.fried.winsRMF

RMFposthoc <- pairwise.wilcox.test(RMF, condition, paired = TRUE, exact = FALSE,  p.adj = "bonferroni", correct = FALSE)
RMFposthoc

wilcox_effsize(df, RMF ~ condition, paired = TRUE)

aggregate(df$LRF, list(df$condition), FUN=mean)
aggregate(df$LRF, list(df$condition), FUN=sd)

aggregate(df$RRF, list(df$condition), FUN=mean)
aggregate(df$RRF, list(df$condition), FUN=sd)

aggregate(df$LBF, list(df$condition), FUN=mean)
aggregate(df$LBF, list(df$condition), FUN=sd)

aggregate(df$RBF, list(df$condition), FUN=mean)
aggregate(df$RBF, list(df$condition), FUN=sd)

aggregate(df$LAB, list(df$condition), FUN=mean)
aggregate(df$LAB, list(df$condition), FUN=sd)

aggregate(df$RAB, list(df$condition), FUN=mean)
aggregate(df$RAB, list(df$condition), FUN=sd)

aggregate(df$LMF, list(df$condition), FUN=mean)
aggregate(df$LMF, list(df$condition), FUN=sd)

aggregate(df$RMF, list(df$condition), FUN=mean)
aggregate(df$RMF, list(df$condition), FUN=sd)


#alternative method
artLRF = art(LRF~ condition + (1|Subject), data=data)
summary(artLRF)
anova(artLRF)
#Post-hoc for sig differences
contrast(emmeans(artlm(artLRF, "condition"), ~ condition), method = "pairwise")

artRRF = art(RRF~ condition + (1|Subject), data=data)
summary(artRRF)
anova(artRRF)
#Post-hoc for sig differences
contrast(emmeans(artlm(artRRF, "condition"), ~ condition), method = "pairwise")

artLBF = art(LBF~ condition + (1|Subject), data=data)
summary(artLBF)
anova(artLBF)
#Post-hoc for sig differences
contrast(emmeans(artlm(artLBF, "condition"), ~ condition), method = "pairwise")


artRBF = art(RBF~ condition + (1|Subject), data=data)
summary(artRBF)
anova(artRBF)
#Post-hoc for sig differences
contrast(emmeans(artlm(artRBF, "condition"), ~ condition), method = "pairwise")

artLAB = art(LAB~ condition + (1|Subject), data=data)
summary(artLAB)
anova(artLAB)
#Post-hoc for sig differences
contrast(emmeans(artlm(artLAB, "condition"), ~ condition), method = "pairwise")

artRAB = art(RAB~ condition + (1|Subject), data=data)
summary(artRAB)
anova(artRAB)
#Post-hoc for sig differences
contrast(emmeans(artlm(artRAB, "condition"), ~ condition), method = "pairwise")

artLMF = art(LMF~ condition + (1|Subject), data=data)
summary(artLMF)
anova(artLMF)
#Post-hoc for sig differences
contrast(emmeans(artlm(artLMF, "condition"), ~ condition), method = "pairwise")

artRMF = art(RMF~ condition + (1|Subject), data=data)
summary(artRMF)
anova(artRMF)
#Post-hoc for sig differences
contrast(emmeans(artlm(artRMF, "condition"), ~ condition), method = "pairwise")
