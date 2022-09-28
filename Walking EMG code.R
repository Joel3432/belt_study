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



#-----------------------2 - load file -------------------------------
# load data from spreadsheet
data <- data.frame(X20220927_BeltWalkingACSMabstract)


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
boxplot(RRA)
hist(RRA)
boxplot(LMF)
hist(LMF)
boxplot(RMF)
hist(RMF)

#normality testing
#Shapiro Wilkes test


shapiro.test(LRF[condition == 2])

shapiro.test(LRF[condition == 4])



shapiro.test(RRF[condition == 2])

shapiro.test(RRF[condition == 4])



shapiro.test(LBF[condition == 2])

shapiro.test(LBF[condition == 4])



shapiro.test(RBF[condition == 2])

shapiro.test(RBF[condition == 4])


shapiro.test(LAB[condition == 2])

shapiro.test(LAB[condition == 4])


shapiro.test(RAB[condition == 2])

shapiro.test(RAB[condition == 4])


shapiro.test(LMF[condition == 2])

shapiro.test(LMF[condition == 4])


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
chart.Correlation(data, histogram = TRUE, method = "pearson")



# repeated measures MANOVA

install.packages("MANOVA.RM")
library(MANOVA.RM)

df = data.frame(subject,condition,LRF,RRF,LBF,RBF,LAB,RAB,LMF,RMF)


emgBelt <- multRM(cbind(LRF, RRF, LBF, RBF, LAB, RAB, LMF, RMF) ~ condition, data = data, 
                  subject = subject, within = condition, iter = 10000, alpha = 0.05, dec = 3)
summary(emgBelt)

class(subject)

#Friendman's Test posthoc 

res.fried.winsLRF <- friedman_test(LRF ~ condition | subject, data = df)
res.fried.winsLRF
#p = 0.00284

# pairwise comparisons
LRFposthoc <- pairwise.wilcox.test(LRF, condition, paired = TRUE, exact = FALSE,  p.adj = "bonferroni", correct = FALSE)
LRFposthoc
# 1 vs 2 = 0.00147
# 1 vs 4 = 0.00034
# 2 vs. 4 = 1.000


res.fried.winsRRF <- friedman_test(RRF ~ condition | subject, data = df)
res.fried.winsRRF
#p = 0.0111

RRFposthoc <- pairwise.wilcox.test(RRF, condition, paired = TRUE, exact = FALSE,  p.adj = "bonferroni", correct = FALSE)
RRFposthoc

#1 vs 2 = 0.0034
#1vs 4 = 0.0025
#2 vs 4 = 1.000

res.fried.winsLBF <- friedman_test(LBF ~ condition | subject, data = df)
res.fried.winsLBF
# p =  0.04478

LBFposthoc <- pairwise.wilcox.test(LBF, condition, paired = TRUE, exact = FALSE,  p.adj = "bonferroni", correct = FALSE)
LBFposthoc
#1v2 = 0.434
#1v4 = 0.011
#2v4 = 0.538

res.fried.winsRBF <- friedman_test(RBF ~ condition | subject, data = df)
res.fried.winsRBF
# p = 0.0724

res.fried.winsLAB <- friedman_test(LAB ~ condition | subject, data = df)
res.fried.winsLAB
#p = 0.130

res.fried.winsRAB <- friedman_test(RAB ~ condition | subject, data = df)
res.fried.winsRAB
# p = 0.130

res.fried.winsLMF <- friedman_test(LMF ~ condition | subject, data = df)
res.fried.winsLMF
# p = 0.0137

LMFposthoc <- pairwise.wilcox.test(LMF, condition, paired = TRUE, exact = FALSE,  p.adj = "bonferroni", correct = FALSE)
LMFposthoc

# 1 vs 2 = 0.0030
# 1 vs 4 = 0.0065
#2 vs 4 = 1.000

res.fried.winsRMF <- friedman_test(RMF ~ condition | subject, data = df)
res.fried.winsRMF
#p = 0.00144

RMFposthoc <- pairwise.wilcox.test(RMF, condition, paired = TRUE, exact = FALSE,  p.adj = "bonferroni", correct = FALSE)
RMFposthoc

# 1vs 2 = 0.05248
# 1 vs 4 = 0.00034
# 2 vs 4 = 1.000

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



