#code for Belt Study Quiet Standing, Dual Communication, and Serial 7's
#written by: Megan Dr. Joel Martin
#August 2022

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

#-----------------------2 - load file -------------------------------
# load data from spreadsheet
data <- read_excel("ArtoolChiDataBook.xlsx", sheet="ARToolSheet_NoNylon")

# for separate Chi-Square analyses, upload the sheets below 

QSdata <- read_excel("ArtoolChiDataBook.xlsx", sheet="QS")

S7data <- read_excel("ArtoolChiDataBook.xlsx", sheet="S7")

DCdata <- read_excel("ArtoolChiDataBook.xlsx", sheet="DC")
 

#-----------------------3 - assign variables ------------------------
subject <- data$Subject
condition <- data$Condition
rangeAP <- data$Range_AP
rangeML <- data$Range_ML
meanvel <- data$Mean_vel
meanvelAP <- data$Mean_vel_AP
meanvelML <- data$Mean_vel_ML
ellipsearea <- data$Ellipse_area_95
LRF <- data$LRF
RRF <- data$RRF
LBF <- data$LBF
RBF <- data$RBF
LAB <- data$LAB
RAB <- data$RAB
LMF <- data$LMF
RMF <- data$RMF
task <- data$Task

#Change to factors 
subject<-as.factor(data$Subject) 
condition<-as.factor(data$Condition) 
task<- as.factor(data$Task) 

#Label conditions 
data$Condition <- factor(data$Condition,levels = c(1,2,3),labels = c("Control", "Leather belt","Vest"))
data$Task <- factor(data$Task,levels = c(1,2,3),labels = c("Quiet Standing", "Serial 7's","Dual Comms"))
#winsorize data already performed in microsoft excel 


df = data.frame(subject,condition,task,LRF,RRF,LBF,RBF,LAB,RAB,LMF,RMF,rangeAP,rangeML,meanvel,meanvelAP,meanvelML,ellipsearea)

#-----------------------4 - explore data  ------------------------

#normality tested in separate code

#Non-parametric ARTool analysis 
#https://depts.washington.edu/acelab/proj/art/

#art transforms the data
#summary to check that art was done properly ( should be 0's)
#run as ANOVA

#Analysis 
artLAB = art(LAB ~ condition * task + (1|subject), data=df)
summary(artLAB)
anova(artLAB)
#Post Hoc Condition Unnecessary 
art.con(artLAB, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Post Hoc Task Unnecessary 
art.con(artLAB, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Post Hoc Interaction
art.con(artLAB, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#interaction plot 
Condition <-df$condition

interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = Condition,
                 response     = df$LAB, ylab = "Left Abdominals RMS Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")



#Analysis
artRAB = art(RAB ~ condition * task + (1|subject), data=df)
summary(artRAB)
anova(artRAB)
#Post Hoc Condition
art.con(artRAB, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Post Hoc Task 
art.con(artRAB, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Post Hoc Interaction Unnecessary 
art.con(artRAB, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Analysis
artLMF = art(LMF ~ condition * task + (1|subject), data=df)
summary(artLMF)
anova(artLMF)
# Post Hoc Task 
art.con(artLMF, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Post Hoc Interaction
art.con(artLMF, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

Condition <-df$condition

interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = Condition,
                 response     = df$LMF, ylab = "Left Multifidus RMS Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

#Analysis 
artRMF = art(RMF ~ condition * task + (1|subject), data=df)
summary(artRMF)
anova(artRMF)
#Post Hoc Condition ##Unnecessary## 
art.con(artRMF, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Post Hoc Task 
art.con(artRMF, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Post Hoc Interaction
art.con(artRMF, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Analysis
artLRF = art(LRF ~ condition * task + (1|subject), data=df)
summary(artLRF)
anova(artLRF)
#Post Hoc Condition
art.con(artLRF, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#Post Hoc Task 
art.con(artLRF, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#Post Hoc Interaction
art.con(artLRF, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Analysis
artRRF = art(RRF ~ condition * task + (1|subject), data=df)
summary(artRRF)
anova(artRRF)

#Post Hoc Condition
art.con(artRRF, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Post Hoc Task 
art.con(artRRF, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#Post Hoc Interaction
art.con(artRRF, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#Analysis
artLBF = art(LBF ~ condition * task + (1|subject), data=df)
summary(artLBF)
anova(artLBF)
#Post Hoc Condition ##unnecessary##
art.con(artLBF, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Post Hoc Task  ##unnecessary##
art.con(artLBF, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#Analysis
artRBF = art(RBF ~ condition * task + (1|subject), data=df)
summary(artRBF)
anova(artRBF)
#Post Hoc Condition
art.con(artRBF, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Analysis
artrangeAP = art(rangeAP ~ condition * task + (1|subject), data=df)
summary(artrangeAP)
anova(artrangeAP)
#Post Hoc Task 
art.con(artrangeAP, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#Analysis
artrangeML = art(rangeML ~ condition * task + (1|subject), data=df)
summary(artrangeML)
anova(artrangeML)
#Post Hoc Task
art.con(artrangeML, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Analysis
artmeanvel = art(meanvel ~ condition * task + (1|subject), data=df)
summary(artmeanvel)
anova(artmeanvel)
#Post Hoc Task 
art.con(artmeanvel, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Analysis
artmeanvelAP = art(meanvelAP ~ condition * task + (1|subject), data=df)
summary(artmeanvelAP)
anova(artmeanvelAP)
#Post Hoc Task 
art.con(artmeanvelAP, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Analysis
artmeanvelML = art(meanvelML ~ condition * task + (1|subject), data=df)
summary(artmeanvelML)
anova(artmeanvelML)
#Post Hoc Task 
art.con(artmeanvelML, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))
#Analysis
artellipse = art(ellipsearea ~ condition * task + (1|subject), data=df)
summary(artellipse)
anova(artellipse)
#Post Hoc Task 
art.con(artellipse, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#Descriptives
aggregate(df$RAB, list(df$task), FUN=mean)
aggregate(df$RRF, list(df$task), FUN=mean)




#Grouped Bar Chart visuals if applicable

ggplot(df, aes(fill=condition, y=LAB, x=task)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(df, aes(fill=condition, y=RAB, x=task)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(df, aes(fill=condition, y=LMF, x=task)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(data, aes(fill=condition, y=RMF, x=task)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(df, aes(fill=condition, y=LRF, x=task)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(df aes(fill=condition, y=RRF, x=task)) + 
  geom_bar(position="dodge", stat="identity") 
  
ggplot(df, aes(fill=condition, y=LBF, x=task)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(df, aes(fill=condition, y=RBF, x=task)) + 
  geom_bar(position="dodge", stat="identity") 

ggplot(df, aes(fill=condition, y=rangeAP, x=task)) + 
  geom_bar(position="dodge", stat="identity") 

ggplot(data, aes(fill=condition, y=rangeML, x=task)) + 
  geom_bar(position="dodge", stat="identity") 

ggplot(df, aes(fill=condition, y=meanvel, x=task)) + 
  geom_bar(position="dodge", stat="identity") 

ggplot(df, aes(fill=condition, y=meanvelAP, x=task)) + 
  geom_bar(position="dodge", stat="identity") 

ggplot(df, aes(fill=condition, y=meanvelML, x=task)) + 
  geom_bar(position="dodge", stat="identity") 

ggplot(df, aes(fill=condition, y=ellipsearea, x=task)) + 
  geom_bar(position="dodge", stat="identity") 

#box plots if applicable (only if individual winsorized sheets are uploaded, not by task)
rangeAPBox <- ggplot(df, aes(group = condition, x = condition, y=`rangeAP`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
rangeAPBox

rangeMLBox <- ggplot(df, aes(group = condition, x = condition, y=`rangeML`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
rangeMLBox

meanvelBox <- ggplot(df, aes(group = condition, x = condition, y=`meanvel`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
meanvelBox

meanvelAPBox <- ggplot(df, aes(group = condition, x = condition, y=`meanvelAP`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
meanvelAPBox

meanvelMLBox <- ggplot(df, aes(group = condition, x = condition, y=`meanvelML`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
meanvelMLBox

ellipseBox <- ggplot(df, aes(group = condition, x = condition, y=`ellipsearea`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
ellipseBox
 
LABBox <- ggplot(df, aes(group = condition, x = condition, y=`LAB`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
LABBox

RABBox <- ggplot(df, aes(group = condition, x = condition, y=`RAB`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
RABBox

LMFBox <- ggplot(df, aes(group = condition, x = condition, y=`LMF`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
LMFBox

RMFBox <- ggplot(df, aes(group = condition, x = condition, y=`RMF`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
RMFBox

LRFBox <- ggplot(df, aes(group = condition, x = condition, y=`LRF`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
LRFBox

RRFBox <- ggplot(df, aes(group = condition, x = condition, y=`RRF`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
RRFBox

LBFBox <- ggplot(df, aes(group = condition, x = condition, y=`LBF`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
LBFBox

RBFBox <- ggplot(df, aes(group = condition, x = condition, y=`RBF`, fill=condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
RBFBox

