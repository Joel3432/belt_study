#code for Belt Study ACSM Abstract EMG Walking
#written by: Megan & Dr. Joel Martin
#Sept 2022

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
library("corrplot")
library("MANOVA.RM")
library(readxl)
library(ARTool)
library(emmeans)    #emmeans, contrast
library(phia)       #testInteractions
library(MANOVA.RM)

#-----------------------2 - load file -------------------------------
# load data from spreadsheet
data <- read_excel("20220929_BeltWalkingACSMabstract.xlsx", sheet="WinRelData99")

#sheet options are; "WinRelData99" "WinRawData99"


#-----------------------3 - assign variables ------------------------
subject <- data$Subject
condition <- data$Condition
LRF <- data$LRF
RRF <- data$RRF
LBF <- data$LBF
RBF <- data$RBF
LAB <- data$LAB
RAB <- data$RAB
LMF <- data$LMF
RMF <- data$RMF



subject<-as.factor(data$Subject) 
condition<-as.factor(data$Condition) 

#LABEL IV
data$Condition <- factor(data$Condition,levels = c(1,2,3,4),labels = c("Control", "Leather belt","Nylon Belt","Vest"))




graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))


#visualize data 

boxplot(LRF)
hist(LRF)
boxplot(RRF)
hist(RRF)
boxplot(LBF)
hist(LBF)
boxplot(RBF)
hist(RBF)
boxplot(LAB)
hist(LAB)
boxplot(RAB)
hist(RAB)
boxplot(LMF)
hist(LMF)
boxplot(RMF)
hist(RMF)

#normality testing
#Shapiro Wilkes test

shapiro.test(LRF)
shapiro.test(LRF[condition == 1])
shapiro.test(LRF[condition == 2])
shapiro.test(LRF[condition == 4])

shapiro.test(RRF)
shapiro.test(RRF[condition == 1])
shapiro.test(RRF[condition == 2])
shapiro.test(RRF[condition == 4])

shapiro.test(LBF)
shapiro.test(LBF[condition == 1])
shapiro.test(LBF[condition == 2])
shapiro.test(LBF[condition == 4])

shapiro.test(RBF)
shapiro.test(RBF[condition == 1])
shapiro.test(RBF[condition == 2])
shapiro.test(RBF[condition == 4])

shapiro.test(LAB)
shapiro.test(LAB[condition == 1])
shapiro.test(LAB[condition == 2])
shapiro.test(LAB[condition == 4])

shapiro.test(RAB)
shapiro.test(RAB[condition == 1])
shapiro.test(RAB[condition == 2])
shapiro.test(RAB[condition == 4])

shapiro.test(LMF)
shapiro.test(LMF[condition == 1])
shapiro.test(LMF[condition == 2])
shapiro.test(LMF[condition == 4])

shapiro.test(RMF)
shapiro.test(RMF[condition == 1])
shapiro.test(RMF[condition == 2])
shapiro.test(RMF[condition == 4])


#examine multicollinearity 

#matrix <- rcorr(as.matrix(data))
# Extract the correlation coefficients
#matrix$r
# Extract p-values
#matrix$P


#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

emgData<-data.frame(data$LRF,data$RRF,data$LBF,data$RBF,data$LAB,data$RAB,data$LMF,data$RMF)
chart.Correlation(emgData, histogram = TRUE, method = "pearson")


# repeated measures MANOVA


df = data.frame(subject,condition,LRF,RRF,LBF,RBF,LAB,RAB,LMF,RMF)


emgBelt <- multRM(cbind(LRF, RRF, LBF, RBF, LAB, RAB, LMF, RMF) ~ condition, data = df, 
                  subject = "subject", within = "condition", iter = 10000, alpha = 0.05, dec = 3)
summary(emgBelt)

library()
#Friedman's Test posthoc 
#added wilcoxon signed ranks test when necessary.
#added effect size calculations when necessary.

#left rectus femoris 
res.fried.winsLRF <- friedman_test(LRF ~ condition | subject, data = df)
res.fried.winsLRF

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
