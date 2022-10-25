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
library(emmeans)
library(multcomp)
library(DescTools)
library(car)

library(rcompanion)

#-----------------------2 - load file -------------------------------
# load data from spreadsheet
ARTooldata <- read_excel("ArtoolChiDataBook.xlsx", sheet="ARToolSheet_NoNylon")



#-----------------------3 - assign variables ------------------------
#ARtool Analysis
subject <- ARTooldata$Subject
condition <- ARTooldata$Condition
rangeAP <- ARTooldata$Range_AP
rangeML <- ARTooldata$Range_ML
meanvel <- ARTooldata$Mean_vel
meanvelAP <- ARTooldata$Mean_vel_AP
meanvelML <- ARTooldata$Mean_vel_ML
ellipsearea <- ARTooldata$Ellipse_area_95
LRF <- ARTooldata$LRF
RRF <- ARTooldata$RRF
LBF <- ARTooldata$LBF
RBF <- ARTooldata$RBF
LAB <- ARTooldata$LAB
RAB <- ARTooldata$RAB
LMF <- ARTooldata$LMF
RMF <- ARTooldata$RMF
task <- ARTooldata$Task
LRF_Rel <- ARTooldata$LRF_Rel
RRF_Rel <- ARTooldata$RRF_Rel
LBF_Rel <- ARTooldata$LBF_Rel
RBF_Rel <- ARTooldata$RBF_Rel
LAB_Rel <- ARTooldata$LAB_Rel
RAB_Rel <- ARTooldata$RAB_Rel
LMF_Rel <- ARTooldata$LMF_Rel
RMF_Rel <- ARTooldata$RMF_Rel


#Change to factors 
subject<-as.factor(ARTooldata$Subject) 
condition<-as.factor(ARTooldata$Condition) 
task<- as.factor(ARTooldata$Task) 

#Label conditions 
condition <- factor(condition,levels = c(1,2,3),labels = c("Control", "Leather belt","Vest"))
task <- factor(task,levels = c(1,2,3),labels = c("Quiet Standing", "Serial 7's","Dual Comms"))
#winsorize data already performed in microsoft excel 


df = data.frame(subject,condition,task,LRF,RRF,LBF,RBF,LAB,RAB,LMF,RMF,rangeAP,rangeML,meanvel,meanvelAP,meanvelML,ellipsearea)

#-----------------------4 - explore data  ------------------------

#normality tested in separate code

#Non-parametric ARTool analysis 
#https://depts.washington.edu/acelab/proj/art/

#art transforms the data
#summary to check that art was done properly ( should be 0's)
#run as ANOVA

#Analysis on Relative data 
#Analysis
artLAB = art(LAB_Rel ~ condition * task + (1|subject), data=df)
summary(artLAB)
anova(artLAB) 


#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartLAB = anova(artLAB)
ResultanovaartLAB$part.eta.sq = with(ResultanovaartLAB, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartLAB


#create lm for cohen's d post hoc calculation later on 
artLAB.linear = lm(LAB_Rel ~ condition*task, data=df)

#Post Hoc Condition 
art.con(artLAB, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " "))) 

artLABES = summary(pairs(emmeans(artLAB.linear, ~ condition)))
artLABES$d = artLABES$estimate / sigmaHat(artLAB.linear)
artLABES

#Post Hoc Task 
art.con(artLAB, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " "))) 

artLABES = summary(pairs(emmeans(artLAB.linear, ~ task)))
artLABES$d = artLABES$estimate / sigmaHat(artLAB.linear)
artLABES

art.con(artLAB, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))  

artLABES = summary(pairs(emmeans(artLAB.linear, ~ condition*task)))
artLABES$d = artLABES$estimate / sigmaHat(artLAB.linear)
artLABES

#interaction plot 


interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$LAB_Rel, ylab = "Left Abdominals Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")


#Analysis
artRAB = art(RAB_Rel ~ condition * task + (1|subject), data=df)
summary(artRAB)
anova(artRAB)

#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartRAB = anova(artRAB)
ResultanovaartRAB$part.eta.sq = with(ResultanovaartRAB, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartRAB

#create lm for cohen's d post hoc calculation later on 
artRAB.linear = lm(RAB_Rel ~ condition*task, data=df)

#Post Hoc Condition
art.con(artRAB, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artRABES = summary(pairs(emmeans(artRAB.linear, ~ condition)))
artRABES$d = artRABES$estimate / sigmaHat(artRAB.linear)
artRABES

#Post Hoc Task 
art.con(artRAB, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artRABES = summary(pairs(emmeans(artRAB.linear, ~ task)))
artRABES$d = artRABES$estimate / sigmaHat(artRAB.linear)
artRABES

#Post Hoc Interaction 
art.con(artRAB, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artRABES = summary(pairs(emmeans(artRAB.linear, ~ condition*task)))
artRABES$d = artRABES$estimate / sigmaHat(artRAB.linear)
artRABES

#interaction plot 


interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$RAB_Rel, ylab = "Right Abdominals Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

#Analysis
artLMF = art(LMF_Rel ~ condition * task + (1|subject), data=df)
summary(artLMF)
anova(artLMF)

#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartLMF = anova(artLMF)
ResultanovaartLMF$part.eta.sq = with(ResultanovaartLMF, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartLMF

#create lm for cohen's d post hoc calculation later on 
artLMF.linear = lm(LMF_Rel ~ condition*task, data=df)

# Post Hoc Condition
art.con(artLMF, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artLMFES = summary(pairs(emmeans(artLMF.linear, ~ condition)))
artLMFES$d = artLMFES$estimate / sigmaHat(artLMF.linear)
artLMFES

# Post Hoc Task 
art.con(artLMF, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artLMFES = summary(pairs(emmeans(artLMF.linear, ~ task)))
artLMFES$d = artLMFES$estimate / sigmaHat(artLMF.linear)
artLMFES

#Post Hoc Interaction
art.con(artLMF, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artLMFES = summary(pairs(emmeans(artLMF.linear, ~ condition*task)))
artLMFES$d = artLMFES$estimate / sigmaHat(artLMF.linear)
artLMFES


interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$LMF_Rel, ylab = "Left Multifidus Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

#Analysis 
artRMF = art(RMF_Rel ~ condition * task + (1|subject), data=df)
summary(artRMF)
anova(artRMF)

#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartRMF = anova(artRMF)
ResultanovaartRMF$part.eta.sq = with(ResultanovaartRMF, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartRMF


#create lm for cohen's d post hoc calculation later on 
artRMF.linear = lm(RMF_Rel ~ condition*task, data=df)

#Post Hoc Condition
art.con(artRMF, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artRMFES = summary(pairs(emmeans(artRMF.linear, ~ condition)))
artRMFES$d = artRMFES$estimate / sigmaHat(artRMF.linear)
artRMFES


#Post Hoc Task 
art.con(artRMF, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artRMFES = summary(pairs(emmeans(artRMF.linear, ~ task)))
artRMFES$d = artRMFES$estimate / sigmaHat(artRMF.linear)
artRMFES

#Post Hoc Interaction
art.con(artRMF, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artRMFES = summary(pairs(emmeans(artRMF.linear, ~ condition*task)))
artRMFES$d = artRMFES$estimate / sigmaHat(artRMF.linear)
artRMFES

#interaction plot 


interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$RMF_Rel, ylab = "Right Multifidus Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

#Analysis
artLRF = art(LRF_Rel ~ condition * task + (1|subject), data=df)
summary(artLRF)
anova(artLRF)

#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartLRF = anova(artLRF)
ResultanovaartLRF$part.eta.sq = with(ResultanovaartLRF, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartLRF


#create lm for cohen's d post hoc calculation later on 
artLRF.linear = lm(LRF_Rel ~ condition*task, data=df)


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

artLRFES = summary(pairs(emmeans(artLRF.linear, ~ task)))
artLRFES$d = artLRFES$estimate / sigmaHat(artLRF.linear)
artLRFES

#Post Hoc Interaction
art.con(artLRF, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artLRFES = summary(pairs(emmeans(artLRF.linear, ~ condition*task)))
artLRFES$d = artLRFES$estimate / sigmaHat(artLRF.linear)
artLRFES
#interaction plot 


interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$LRF_Rel, ylab = "Left Rectus Femoris Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

#Analysis
artRRF = art(RRF_Rel ~ condition * task + (1|subject), data=df)
summary(artRRF)
anova(artRRF)

#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartRRF = anova(artRRF)
ResultanovaartRRF$part.eta.sq = with(ResultanovaartRRF, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartRRF

#create lm for cohen's d post hoc calculation later on 
artRRF.linear = lm(RRF_Rel ~ condition*task, data=df)

#Post Hoc Condition
art.con(artRRF, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artRRFES = summary(pairs(emmeans(artRRF.linear, ~ condition)))
artRRFES$d = artRRFES$estimate / sigmaHat(artRRF.linear)
artRRFES

#Post Hoc Task 
art.con(artRRF, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artRRFES = summary(pairs(emmeans(artRRF.linear, ~ task)))
artRRFES$d = artRRFES$estimate / sigmaHat(artRRF.linear)
artRRFES

#Post Hoc Interaction
art.con(artRRF, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#interaction plot 


interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$RRF_Rel, ylab = "Right Rectus Femoris Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")


#Analysis
artLBF = art(LBF_Rel ~ condition * task + (1|subject), data=df)
summary(artLBF)
anova(artLBF)

#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartLBF = anova(artLBF)
ResultanovaartLBF$part.eta.sq = with(ResultanovaartLBF, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartLBF

#create lm for cohen's d post hoc calculation later on 
artLBF.linear = lm(LBF_Rel ~ condition*task, data=df)

#Post Hoc Condition 
art.con(artLBF, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#Post Hoc Task
art.con(artLBF, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artLBFES = summary(pairs(emmeans(artLBF.linear, ~ task)))
artLBFES$d = artLBFES$estimate / sigmaHat(artLBF.linear)
artLBFES

#Post Hoc Interaction
art.con(artLBF, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#interaction plot 


interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$LBF_Rel, ylab = "Left Biceps Femoris Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")



#Analysis
artRBF = art(RBF_Rel ~ condition * task + (1|subject), data=df)
summary(artRBF)
anova(artRBF)

#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartRBF = anova(artRBF)
ResultanovaartRBF$part.eta.sq = with(ResultanovaartRBF, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartRBF

#create lm for cohen's d post hoc calculation later on 
artRBF.linear = lm(RBF_Rel ~ condition*task, data=df)

#Post Hoc Condition
art.con(artRBF, ~condition, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artRBFES = summary(pairs(emmeans(artRBF.linear, ~ condition)))
artRBFES$d = artRBFES$estimate / sigmaHat(artRBF.linear)
artRBFES


#Post Hoc Task
art.con(artRBF, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#Post Hoc Interaction
art.con(artRBF, "condition:task", adjust="bonferroni") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#interaction plot 


interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$RBF_Rel, ylab = "Right Biceps Femoris Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")



#Analysis
artrangeAP = art(rangeAP ~ condition * task + (1|subject), data=df)
summary(artrangeAP)
anova(artrangeAP)

#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartrangeAP = anova(artrangeAP)
ResultanovaartrangeAP$part.eta.sq = with(ResultanovaartrangeAP, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartrangeAP

#create lm for cohen's d post hoc calculation later on 
artrangeAP.linear = lm(rangeAP ~ condition*task, data=df)

#Post Hoc Task 
art.con(artrangeAP, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artrangeAPES = summary(pairs(emmeans(artrangeAP.linear, ~ task)))
artrangeAPES$d = artrangeAPES$estimate / sigmaHat(artrangeAP.linear)
artrangeAPES

#Analysis
artrangeML = art(rangeML ~ condition * task + (1|subject), data=df)
summary(artrangeML)
anova(artrangeML)

#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartrangeML = anova(artrangeML)
ResultanovaartrangeML$part.eta.sq = with(ResultanovaartrangeML, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartrangeML

#create lm for cohen's d post hoc calculation later on 
artrangeML.linear = lm(rangeML ~ condition*task, data=df)

#Post Hoc Task
art.con(artrangeML, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artrangeMLES = summary(pairs(emmeans(artrangeML.linear, ~ task)))
artrangeMLES$d = artrangeMESL$estimate / sigmaHat(artrangeML.linear)
artrangeMLES

#Analysis
artmeanvel = art(meanvel ~ condition * task + (1|subject), data=df)
summary(artmeanvel)
anova(artmeanvel)

#calculate factorial ANOVA effect size -- partial eta squared 
Resultanovaartmeanvel = anova(artmeanvel)
Resultanovaartmeanvel$part.eta.sq = with(Resultanovaartmeanvel, `F` * `Df` / (`F` * `Df` + `Df.res`))
Resultanovaartmeanvel

#create lm for cohen's d post hoc calculation later on 
artmeanvel.linear = lm(meanvel ~ condition*task, data=df)

#Post Hoc Task 
art.con(artmeanvel, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artmeanvelES = summary(pairs(emmeans(artmeanvel.linear, ~ task)))
artmeanvelES$d = artmeanvelES$estimate / sigmaHat(artmeanvel.linear)
artmeanvelES

#Analysis
artmeanvelAP = art(meanvelAP ~ condition * task + (1|subject), data=df)
summary(artmeanvelAP)
anova(artmeanvelAP)

#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartmeanvelAP = anova(artmeanvelAP)
ResultanovaartmeanvelAP$part.eta.sq = with(ResultanovaartmeanvelAP, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartmeanvelAP

#create lm for cohen's d post hoc calculation later on 
artmeanvelAP.linear = lm(meanvelAP ~ condition*task, data=df)

#Post Hoc Task 
art.con(artmeanvelAP, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artmeanvelAPES = summary(pairs(emmeans(artmeanvelAP.linear, ~ task)))
artmeanvelAPES$d = artmeanvelAPES$estimate / sigmaHat(artmeanvelAP.linear)
artmeanvelAPES

#Analysis
artmeanvelML = art(meanvelML ~ condition * task + (1|subject), data=df)
summary(artmeanvelML)
anova(artmeanvelML)

#calculate factorial ANOVA effect size -- partial eta squared 
ResultanovaartmeanvelML = anova(artmeanvelML)
ResultanovaartmeanvelML$part.eta.sq = with(ResultanovaartmeanvelML, `F` * `Df` / (`F` * `Df` + `Df.res`))
ResultanovaartmeanvelML

#create lm for cohen's d post hoc calculation later on 
artmeanvelML.linear = lm(meanvelML ~ condition*task, data=df)

#Post Hoc Task 
art.con(artmeanvelML, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artmeanvelMLES = summary(pairs(emmeans(artmeanvelML.linear, ~ task)))
artmeanvelMLES$d = artmeanvelMLES$estimate / sigmaHat(artmeanvelML.linear)
artmeanvelMLES

#Analysis
artellipse = art(ellipsearea ~ condition * task + (1|subject), data=df)
summary(artellipse)
anova(artellipse)

#calculate factorial ANOVA effect size -- partial eta squared 
Resultanovaartellipse = anova(artellipse)
Resultanovaartellipse$part.eta.sq = with(Resultanovaartellipse, `F` * `Df` / (`F` * `Df` + `Df.res`))
Resultanovaartellipse


#create lm for cohen's d post hoc calculation later on 
artellipse.linear = lm(ellipsearea ~ condition*task, data=df)

#Post Hoc Task 
art.con(artellipse, ~task, adjust="bonferroni") %>%  # run ART-C for X1
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " ")))

artellipseES = summary(pairs(emmeans(artellipse.linear, ~ task)))
artellipseES$d = artellipseES$estimate / sigmaHat(artellipse.linear)
artellipseES

#Descriptives
# use code below for means and SDs. Change to muscle of COP of interest. 
#Change list to condition or task 
aggregate(df$LAB, list(df$task), FUN=mean)
aggregate(df$LAB, list(df$task), FUN=sd)


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

ggplot(df, aes(fill=condition, y=RRF, x=task)) + 
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



#Interaction plots together 

# 4 figures arranged in 2 rows and 2 columns
attach(mtcars)
par(mfrow=c(2,2))
interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$LAB_Rel, ylab = "Left Abdominals Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")
interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$RAB_Rel, ylab = "Right Abdominals Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")
interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$LMF_Rel, ylab = "Left Multifidus Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")
interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$RMF_Rel, ylab = "Right Multifidus Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")


par(mfrow=c(3,1))
interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$LRF_Rel, ylab = "Left Rectus Femoris Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")
interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$RRF_Rel, ylab = "Right Rectus Femoris Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

interaction.plot(x.factor     = df$task, xlab = "Task",
                 trace.factor = condition,
                 response     = df$RBF_Rel, ylab = "Right Biceps Femoris Activity",
                 fun = mean,
                 type="b",
                 col=c("black","dark gray","light gray"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")



##### NOT APPLICABLE. JUST KEEPING FOR REFERENCE IN FUTURE ANALYSIS ###
#######################################################################
#######################################################################

#######################################################################


#Load data for chi square analysis 
QSdata <- read_excel("ArtoolChiDataBook.xlsx", sheet="QS")

S7data <- read_excel("ArtoolChiDataBook.xlsx", sheet="S7")

DCdata <- read_excel("ArtoolChiDataBook.xlsx", sheet="DC")

#assess chi-square to see if there is a significant difference in distribution of those who increased
#and decreased COP between conditions for each task

table(EllipseCondition, QSEllipseChange)
chisq.test(EllipseCondition, QSEllipseChange, correct=FALSE)

table(EllipseCondition, S7EllipseChange)
chisq.test(EllipseCondition, S7EllipseChange, correct=FALSE)

table(EllipseCondition, DCEllipseChange)
chisq.test(EllipseCondition, DCEllipseChange, correct=FALSE)


#load data to assess differences between groups who increased and decreased COP per condition

EllipseChangeVest <- read_excel("ArtoolChiDataBook.xlsx", sheet="EllipseChangeVest")
EllipseChangeBelt <- read_excel("ArtoolChiDataBook.xlsx", sheet="EllipseChangeBelt")

#Assess differences in muscular activity between those who increased and decreased COP during vest condition

wilcox.test(EllipseChangeVest$QSLAB ~ EllipseChangeVest$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$QSRAB ~ EllipseChangeVest$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$QSLMF ~ EllipseChangeVest$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$QSRMF ~ EllipseChangeVest$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$QSLRF ~ EllipseChangeVest$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$QSRRF ~ EllipseChangeVest$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$QSLBF ~ EllipseChangeVest$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$QSRBF ~ EllipseChangeVest$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

wilcox.test(EllipseChangeVest$S7LAB ~ EllipseChangeVest$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$S7RAB ~ EllipseChangeVest$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$S7LMF ~ EllipseChangeVest$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$S7RMF ~ EllipseChangeVest$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$S7LRF ~ EllipseChangeVest$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$S7RRF ~ EllipseChangeVest$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$S7LBF ~ EllipseChangeVest$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$S7RBF ~ EllipseChangeVest$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

wilcox.test(EllipseChangeVest$DCLAB ~ EllipseChangeVest$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$DCRAB ~ EllipseChangeVest$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$DCLMF ~ EllipseChangeVest$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$DCRMF ~ EllipseChangeVest$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$DCLRF ~ EllipseChangeVest$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$DCRRF ~ EllipseChangeVest$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$DCLBF ~ EllipseChangeVest$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeVest$DCRBF ~ EllipseChangeVest$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

#analyze mean and SD of LAB 
aggregate(EllipseChangeVest$DCLAB, list(EllipseChangeVest$DCEllipseChange), FUN=mean)
aggregate(EllipseChangeVest$DCLAB, list(EllipseChangeVest$DCEllipseChange), FUN=sd)

#assign variables 
DCEllipseChangeVest <- EllipseChangeVest$DCEllipseChange
DCLabVest<-EllipseChangeVest$DCLAB
#create new data frame for effect size calculation
effectsizedf <- data.frame(DCEllipseChangeVest, DCLabVest)
#calculate effect size 
wilcox_effsize(effectsizedf, DCLabVest ~ DCEllipseChangeVest, paired = FALSE)


#Assess differences in muscular activity between those who increased and decreased COP during belt condition

wilcox.test(EllipseChangeBelt$QSLAB ~ EllipseChangeBelt$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$QSRAB ~ EllipseChangeBelt$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$QSLMF ~ EllipseChangeBelt$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$QSRMF ~ EllipseChangeBelt$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$QSLRF ~ EllipseChangeBelt$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$QSRRF ~ EllipseChangeBelt$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$QSLBF ~ EllipseChangeBelt$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$QSRBF ~ EllipseChangeBelt$QSEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

wilcox.test(EllipseChangeBelt$S7LAB ~ EllipseChangeBelt$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$S7RAB ~ EllipseChangeBelt$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$S7LMF ~ EllipseChangeBelt$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$S7RMF ~ EllipseChangeBelt$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$S7LRF ~ EllipseChangeBelt$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$S7RRF ~ EllipseChangeBelt$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$S7LBF ~ EllipseChangeBelt$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$S7RBF ~ EllipseChangeBelt$S7EllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

wilcox.test(EllipseChangeBelt$DCLAB ~ EllipseChangeBelt$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$DCRAB ~ EllipseChangeBelt$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$DCLMF ~ EllipseChangeBelt$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$DCRMF ~ EllipseChangeBelt$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$DCLRF ~ EllipseChangeBelt$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$DCRRF ~ EllipseChangeBelt$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$DCLBF ~ EllipseChangeBelt$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
wilcox.test(EllipseChangeBelt$DCRBF ~ EllipseChangeBelt$DCEllipseChange,na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

