---
  title: "Andy's Analsysis of CRQA for Dissertation Experiment 3"
  author:"Andy Tucker"
---
  
### Libraries and data setup -----------------------------------
  library(ggplot2)
  library(lme4)
  library(shiny)
  library(crqa)
  library(lmerTest)
  library(afex)
  library(shiny)
  library(reshape2)
  library(ez)
  library(MuMIn)
  library(multcomp)
  library(stats)
  library(car)

# Set working directory and grab the file
  setwd("P:/Dissertation - HKB-ST/Data/Exp 3")
  crqaData <- read.csv("EXP3 - CRQAoutput.csv")
  names(crqaData)
  
  
# Add ST conditions (Already did this in CRQA script, it should be in CRQAoutput.csv. But do check if reusing this for other experiments!)
#  crqaData$ST <- vector(mode="numeric",length=NROW(crqaData))
#  crqaData$ST <- substr(crqaData$filesInWd, 2,2)
  crqaData$ST <- factor(crqaData$ST)
  crqaData$ID <- factor(crqaData$ID)
  crqaData$Trial <- substr(crqaData$filesInWd, 9,10)
  crqaData$Pend <- factor(crqaData$Pendulums)
  crqaData$Speed <- factor(crqaData$Speed)
  
### Modeling %RECUR ---------------------------------

# Models for Recur%
  m000 <- lmer(rr ~ 1 + (1|ID), data = crqaData)
  summary(m000)
  anova(m000)
  
  m001 <- lmer(rr ~ ST + (1|ID), data = crqaData)
  summary(m001)
  anova(m001)
  
  m002 <- lmer(rr ~ Speed + (1|ID), data = crqaData)
  summary(m002)
  anova(m002)
  r.squaredGLMM(m002)
  
  m003 <- lmer(rr ~ Speed + ST + (1|ID), data = crqaData)
  summary(m003)
  anova(m003)
  
  m004 <- lmer(rr ~ Speed + ST + Speed*ST + (1|ID), data = crqaData)
  summary(m004)
  anova(m004)
  
  m005 <- lmer(rr ~ Pendulums + (1|ID), data = crqaData)
  summary(m005)
  anova(m005)
  
  m006 <- lmer(rr ~ ST + Speed + Pendulums + ST*Speed*Pendulums + (1|ID), data = crqaData)
  summary(m006)
  anova(m006)
  r.squaredGLMM(m006)
  
  m007 <- lmer(rr ~ Speed + Pendulums + Speed*Pendulums + (1|ID), data = crqaData)
  summary(m007)
  anova(m007)
  r.squaredGLMM(m007)
  
  anova(m006,m007)
  anova(m007, m002)
  anova(m006,m002)
  

  
  

### Modeling MaxLine -------------------------------------------
  m100 <- lmer(maxLine ~ 1 + (1|ID), data = crqaData)
  summary(m100)
  anova(m100)
  
  m101 <- lmer(maxLine ~ ST + (1|ID), data = crqaData)
  summary(m101)
  anova(m101)
  
  m102 <- lmer(maxLine ~ Speed + (1|ID), data = crqaData)
  summary(m102)
  anova(m102)
  r.squaredGLMM(m102)
  
  m103 <- lmer(maxLine ~ Speed + ST + (1|ID), data = crqaData)
  summary(m103)
  anova(m103)
  
  m104 <- lmer(maxLine ~ Speed + ST + Speed*ST + (1|ID), data = crqaData)
  summary(m104)
  anova(m104)
  
  m105 <- lmer(maxLine ~ Pendulums + (1|ID), data = crqaData)
  summary(m105)
  anova(m105)
  
  m106 <- lmer(maxLine ~ Speed + Pendulums + ST + Speed*Pendulums*ST + (1|ID), data = crqaData)
  summary(m106)
  anova(m106)
  r.squaredGLMM(m106)
  anova(m102,m106)
  
  
  
  # Models for Det
  m200 <- lmer(det ~ 1 + (1|ID), data = crqaData)
  summary(m200)
  anova(m200)
  
  m201 <- lmer(det ~ ST + (1|ID), data = crqaData)
  summary(m201)
  anova(m201)
  
  m202 <- lmer(det ~ Speed + (1|ID), data = crqaData)
  summary(m202)
  anova(m202)
  
  m203 <- lmer(det ~ Speed + ST + (1|ID), data = crqaData)
  summary(m203)
  anova(m203)
  
  m204 <- lmer(det ~ Speed + ST + Speed*ST + (1|ID), data = crqaData)
  summary(m204)
  anova(m204)
  
  m205 <- lmer(det ~ Speed + ST + Pendulums + (1|ID), data = crqaData)
  summary(m205)
  anova(m205)
  

  
  r.squaredGLMM(m106)
  
  
### Descriptives for graphing, etc --------------------------
  
  groupSpeed1 <- subset(crqaData, Speed=="120")
  groupSpeed2 <- subset(crqaData, Speed=="180")
  groupSpeed3 <- subset(crqaData, Speed=="240")
  
  mean(groupSpeed1$rr)
  sd(groupSpeed1$rr)
  mean(groupSpeed2$rr)
  sd(groupSpeed2$rr)
  mean(groupSpeed3$rr)
  sd(groupSpeed3$rr)
  
  groupPendM <- subset(crqaData, Pendulums=="M")
  groupPendR <- subset(crqaData, Pendulums=="R")
  groupPendL <- subset(crqaData, Pendulums=="L")
  
  mean(groupPendM$rr)
  sd(groupPendM$rr)
  mean(groupPendR$rr)
  sd(groupPendR$rr)
  mean(groupPendL$rr)
  sd(groupPendL$rr)
  
  
  mean(groupSpeed1$maxLine)
  sd(groupSpeed1$maxLine)
  mean(groupSpeed2$maxLine)
  sd(groupSpeed2$maxLine)
  mean(groupSpeed3$maxLine)
  sd(groupSpeed3$maxLine)
### Post-hocs ---------------------------
  
  # %RECUR
  summary(glht(m006, linfct=mcp(Speed="Tukey")))
  summary(glht(m006, linfct=mcp(Pendulums="Tukey")))
  
  # MAXLINE
  summary(glht(m106, linfct=mcp(Speed="Tukey")))