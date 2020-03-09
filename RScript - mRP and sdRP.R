---
  title: "Andy's meanRP and sdRP Analysis for Dissertation Experiment 3"
  author:"Andy Tucker"
---
  
### Libraries & Data setup -----------------------------------------
  library(ggplot2)
  library(lme4)
  library(shiny)
  library(crqa)
  library(lmerTest)
  library(afex)
  library(shiny)
  library(reshape2)
  library(ez)
  library(hexbin)
  library(ppcor)
  library(MuMIn)
  library(multcomp)
  library(stats)
  library(car)
  
  
## Set working directory and load relative phase data
  setwd("C:/Users/Andy/Documents/Projects/Dissertation - HKB-ST/Data/Exp 3")
  RPdata <- read.csv("EXP3 - crp_summary.csv", header=TRUE)
  names(RPdata)

## Draw Trial numbers, Speed, and Pendulums from File name.   
  RPdata$Trial <- substr(RPdata$File, 9,10)
  RPdata$Speed <- substr(RPdata$File,14,16)
  RPdata$Pendulums <- substr(RPdata$File, 17,17)
  RPdata$ST <- vector(mode="numeric",length=NROW(RPdata))
  RPdata$ID <- vector(mode="numeric",length=NROW(RPdata))
  RPdata$ST <- substr(RPdata$File, 2,2)
  RPdata$ID <- substr(RPdata$File, 2,4)
  
  RPdata$Pendulums <- factor(RPdata$Pendulums)
  RPdata$ST <- factor(RPdata$ST)
  RPdata$ID <- factor(RPdata$ID)
  RPdata$Speed <- factor(RPdata$Speed)
  
## Take a peek to make sure it worked!  
  #RPdata

  
## Get absolute value of deviation from intended phase (180)
  RPdata$AbsDev <- RPdata$mRP - 180
  RPdata$AbsDev <- abs(RPdata$AbsDev)
  
  
### Modeling SDRP ------------------------------
  m000 <- lmer(sdRP ~ 1 + (1|ID), data = RPdata)
  summary(m000)
  anova(m000)
  
  m001 <- lmer(sdRP ~ 1 + ST + (1|ID), data = RPdata)
  summary(m001)
  anova(m001)
  # Works!
  
  m002 <- lmer(sdRP ~ 1 + Speed + (1|ID), data = RPdata)
  summary(m002)
  anova(m002)
  # Woks!
  
  m003 <- lmer(sdRP ~ 1 + Pendulums + (1|ID), data = RPdata)
  summary(m003)
  anova(m003)
  # Doesn't work?
  
  m004 <- lmer(sdRP ~ 1 + Speed + Pendulums + ST + Speed*Pendulums*ST + (1|ID), data = RPdata)
  summary(m004)
  anova(m004)
  # When you throw everything in, Speed works (p<.001), ST works (p = .009), Speed*ST works (p = .012), and Pendulums*ST is marginal (p = .096). 
  r.squaredGLMM(m004)
  # r^2c = .735
  
### Modeling mRP and Abs Deviation from Intended Phase ------------------------------
  m201 <- lmer(AbsDev ~ 1 + (1|ID), data = RPdata)
  summary(m201)
  anova(m201)
  
  m301 <- lmer(mRP ~ 1 + Speed + Pendulums + ST + Speed*Pendulums*ST + (1|ID), data = RPdata)
  summary(m301)
  anova(m301)
  r.squaredGLMM(m301)
  
  
### Descriptives and Graphing ----------------------------------
  
  subST0 <- subset(RPdata, ST=="1")
  subST1 <- subset(RPdata, ST=="2")
  
  mean(subST0$sdRP)
  sd(subST0$sdRP)
  mean(subST1$sdRP)
  sd(subST1$sdRP)
  
  subSpeed1 <- subset(RPdata, Speed=="120")
  subSpeed2 <- subset(RPdata, Speed=="180")
  subSpeed3 <- subset(RPdata, Speed=="240")
  
  mean(subSpeed1$sdRP)
  sd(subSpeed1$sdRP)
  mean(subSpeed2$sdRP)
  sd(subSpeed2$sdRP)
  mean(subSpeed3$sdRP)
  sd(subSpeed3$sdRP)
  
  subPend1 <- subset(RPdata, Pendulums=="M")
  subPend2 <- subset(RPdata, Pendulums=="R")
  subPend3 <- subset(RPdata, Pendulums=="L")
  
  
  mean(subPend1$sdRP)
  sd(subPend1$sdRP)
  mean(subPend2$sdRP)
  sd(subPend2$sdRP)
  mean(subPend3$sdRP)
  sd(subPend3$sdRP)
  

  subST0Pend1 <- subset(subST0, Pendulums=="M")
  subST0Pend2 <- subset(subST0, Pendulums=="R")
  subST0Pend3 <- subset(subST0, Pendulums=="L")
  
  subST1Pend1 <- subset(subST1, Pendulums=="M")
  subST1Pend2 <- subset(subST1, Pendulums=="R")
  subST1Pend3 <- subset(subST1, Pendulums=="L")
  
  mean(subST0Pend1$mRP)
  sd(subST0Pend1$mRP)
  mean(subST0Pend2$mRP)
  sd(subST0Pend2$mRP)
  mean(subST0Pend3$mRP)
  sd(subST0Pend3$mRP)
  
  mean(subST1Pend1$mRP)
  sd(subST1Pend1$mRP)
  mean(subST1Pend2$mRP)
  sd(subST1Pend2$mRP)
  mean(subST1Pend3$mRP)
  sd(subST1Pend3$mRP)
  
  mean(subST0Pend1$sdRP)
  mean(subST0Pend2$sdRP)
  mean(subST0Pend3$sdRP)
  mean(subST1Pend1$sdRP)
  mean(subST1Pend2$sdRP)
  mean(subST1Pend3$sdRP)
  
  sd(subST0Pend1$sdRP)
  sd(subST0Pend2$sdRP)
  sd(subST0Pend3$sdRP)
  sd(subST1Pend1$sdRP)
  sd(subST1Pend2$sdRP)
  sd(subST1Pend3$sdRP)
  
  
  subST0Speed1 <- subset(subST0, Speed=="120")
  subST0Speed2 <- subset(subST0, Speed=="180")
  subST0Speed3 <- subset(subST0, Speed=="240")
  
  subST1Speed1 <- subset(subST1, Speed=="120")
  subST1Speed2 <- subset(subST1, Speed=="180")
  subST1Speed3 <- subset(subST1, Speed=="240")
  
  mean(subST0Speed1$sdRP)
  sd(subST0Speed1$sdRP)
  mean(subST0Speed2$sdRP)
  sd(subST0Speed2$sdRP)
  mean(subST0Speed3$sdRP)
  sd(subST0Speed3$sdRP)
  
  mean(subST1Speed1$sdRP)
  sd(subST1Speed1$sdRP)
  mean(subST1Speed2$sdRP)
  sd(subST1Speed2$sdRP)
  mean(subST1Speed3$sdRP)
  sd(subST1Speed3$sdRP)
  
  
  
  
## Subsetting further so it's possible to see speed * pendulum interaction
  sub11 <- subset(subSpeed1, Pendulums=="M")
  sub12 <- subset(subSpeed1, Pendulums=="R")
  sub13 <- subset(subSpeed1, Pendulums=="L")
  
  mean(sub11$sdRP)
  mean(sub12$sdRP)
  mean(sub13$sdRP)
  
  sd(sub11$sdRP)
  sd(sub12$sdRP)
  sd(sub13$sdRP)
  
  sub21 <- subset(subSpeed2, Pendulums=="M")
  sub22 <- subset(subSpeed2, Pendulums=="R")
  sub23 <- subset(subSpeed2, Pendulums=="L")
  
  mean(sub21$sdRP)
  mean(sub22$sdRP)
  mean(sub23$sdRP)
  
  sd(sub21$sdRP)
  sd(sub22$sdRP)
  sd(sub23$sdRP)
  
  sub31 <- subset(subSpeed3, Pendulums=="M")
  sub32 <- subset(subSpeed3, Pendulums=="R")
  sub33 <- subset(subSpeed3, Pendulums=="L")
  
  mean(sub31$sdRP)
  mean(sub32$sdRP)
  mean(sub33$sdRP)
  
  sd(sub31$sdRP)
  sd(sub32$sdRP)
  sd(sub33$sdRP)
  
  
  
######### OPTIONAL: Also grab the survey data 
  SurveyData <- read.csv("EXP3 - STactivation and PartnerRating.csv", header=TRUE)
  names(SurveyData)
  
  
## Creating empty columns in RP data to store values transfered from SurveyData 
  RPdata$STactivationDiff <- NA
  RPdata$STactivationMean <- NA
  RPdata$PartnerRatingDiff <- NA
  RPdata$PartnerRatingMean <- NA
  RPdata$STactivation1 <- NA
  RPdata$STactivation2 <- NA
  RPdata$PartnerRating1 <- NA
  RPdata$PartnerRating2 <- NA

 
## Grabbing IDs of each row in RPData, and getting the appropriate values from SurveyData by it.NOTE: Won't work if IDs are factors
  for(i in 1:nrow(RPdata)){
    holdID <- RPdata[i, 12]
    print(holdID)
    
    RPdata[i, 13] <- SurveyData$STactivationDiff[SurveyData$OriginalID == holdID]
    RPdata[i, 14] <- SurveyData$STactivationMean[SurveyData$OriginalID == holdID]
    RPdata[i, 15] <- SurveyData$PartnerRatingDiff[SurveyData$OriginalID == holdID]
    RPdata[i, 16] <- SurveyData$PartnerRatingMean[SurveyData$OriginalID == holdID]
    RPdata[i, 17] <- SurveyData$STactivation1[SurveyData$OriginalID == holdID]
    RPdata[i, 18] <- SurveyData$STactivation2[SurveyData$OriginalID == holdID]
    RPdata[i, 19] <- SurveyData$PartnerRating1[SurveyData$OriginalID == holdID]
    RPdata[i, 20] <- SurveyData$PartnerRating2[SurveyData$OriginalID == holdID]
  }
  
  #### You can write out to a csv at this point for use with David Kenney's shiny apps, http://davidakenny.net/DyadR/DyadRweb.htm
  write.csv(RPdata,"C:/Users/Andy/Documents/Projects/Dissertation - HKB-ST/Data/Exp 3/EXP3 - RPSurveyComboData.csv", row.names = FALSE)
  ####
  
  RPdata$Pendulums <- factor(RPdata$Pendulums)
  RPdata$ST <- factor(RPdata$ST)
  RPdata$ID <- factor(RPdata$ID)
  RPdata$Speed <-factor(RPdata$Speed)

## Modeling RP data with Survey Data
  m100 <- lmer(sdRP ~ 1  + (1|ID), data = RPdata)
  summary(m100)
  anova(m100)
  
  m101 <- lmer(sdRP ~ STactivationMean  + (1|ID), data = RPdata)
  summary(m101)
  anova(m101)
  # STactivationMean predicts sdRP, p = .0218
  
  m102 <- lmer(sdRP ~ PartnerRatingMean  + (1|ID), data = RPdata)
  summary(m102)
  anova(m102)
  # PartingRatingMean predicts sdRP, p = .0442
  
  pcor.test(as.numeric(RPdata$PartnerRatingMean), as.numeric(RPdata$sdRP), as.numeric(RPdata$ID), method = c("pearson"))
  
  

  
  
  
  
 
  
  
  
  
  
### Post-hocs and Simple Main Effects --------------------------------------
  
  #sdRP stuff
  summary(glht(m004, linfct=mcp(Speed="Tukey")))
  
  sm001 <- lmer(sdRP ~ 1 + Speed + (1|ID), data = subST0)
  summary(sm001)
  anova(sm001)
  
  sm002 <- lmer(sdRP ~ 1 + Speed + (1|ID), data = subST1)
  summary(sm002)
  anova(sm002)
  
  summary(glht(sm001, linfct=mcp(Speed="Tukey")))
  summary(glht(sm002, linfct=mcp(Speed="Tukey")))
  
  sm003 <- lmer(sdRP ~ 1 + Pendulums + (1|ID), data = subST0)
  summary(sm003)
  anova(sm003)
  
  sm004 <- lmer(sdRP ~ 1 + Pendulums + (1|ID), data = subST1)
  summary(sm004)
  anova(sm004)
  
  
  #mRP stuff
  summary(glht(m301, linfct=mcp(Pendulums="Tukey")))
  
  sm101 <- lmer(mRP ~ 1 + Pendulums + (1|ID), data = subST0)
  summary(sm101)
  anova(sm101)
  
  summary(glht(sm101, linfct=mcp(Pendulums="Tukey")))
  
  sm102 <- lmer(mRP ~ 1 + Pendulums + (1|ID), data = subST1)
  summary(sm102)
  anova(sm102)
  
  sm103 <- lmer(mRP ~ 1 + Speed + (1|ID), data = subST1)
  summary(sm103)
  anova(sm103)
  
  sm104 <- lmer(mRP ~ 1 + Speed + (1|ID), data = subST1)
  summary(sm104)
  anova(sm104)
  
  sm105 <- lmer(mRP ~ 1 + Speed + (1|ID), data = subPend1)
  summary(sm105)
  anova(sm105)
  
  sm106 <- lmer(mRP ~ 1 + Speed + (1|ID), data = subPend2)
  summary(sm106)
  anova(sm106)
  
  sm107 <- lmer(mRP ~ 1 + Speed + (1|ID), data = subPend3)
  summary(sm107)
  anova(sm107)