---
  title: "Andy's Analsysis of RelPhase bins for Dissertation Experiment 3"
  author:"Andy Tucker"
---
  
### Data setup, libraries for fun & Proffitt --------------------------
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
  library(MuMIn)
  library(multcomp)
  library(stats)
  library(car)

  # Set working directory and grab the file
  setwd("P:/Dissertation - HKB-ST/Data/Exp 3")
  crpbinData <- read.csv("EXP3 - crp_360_counts.csv", header = FALSE)
  
  # Add sensible names.
  newNames <- c("File", "X10", "X30", "X50", "X70", "X90", "X110", "X130", "X150", "X170", "X190", "X210", "X230", "X250", "X270", "X290", "X310", "X330", "X350")
  names(crpbinData) <- newNames
  
  # Draw Trial numbers, Speed, and Pendulums from File name.   
  crpbinData$Trial <- substr(crpbinData$File, 9,10)
  crpbinData$Speed <- substr(crpbinData$File,14,16)
  crpbinData$Pendulums <- substr(crpbinData$File, 17,17)
  crpbinData$Pendulums <- factor(crpbinData$Pendulums)

  # Add ST conditions
  crpbinData$ST <- vector(mode="numeric",length=NROW(crpbinData))
  crpbinData$ID <- vector(mode="numeric",length=NROW(crpbinData))
  crpbinData$ST <- substr(crpbinData$File, 2,2)
  crpbinData$ID <- substr(crpbinData$File, 2,4)
  crpbinData$ST <- factor(crpbinData$ST)
  crpbinData$ID <- factor(crpbinData$ID)
  
  # Go from Wide to Long Format
  crpbinDataLong <- melt(crpbinData)
    # File = file name for tria
    # Trial = trial number
    # Speed = Target pendulum movement frequency (in bpm)
    # Pendulums = pendulum set used. M for matched, R for Right Long, L for Left Long
    # ST = Stereotype threat condition. 0 for control, 1 for ST.
    # ID = id numbers
    # Bin = Relative phase bin. 360* divided into 18 bins
    # TimeSpent = percentage of trial time spent in each bin. 
  longNames <- c("File", "Trial", "Speed", "Pendulums", "ST", "ID", "Bin", "TimeSpent")
  names(crpbinDataLong) <- longNames
  
  # Creat "nearAnti" and "nearIn" bins that combine X170&X190 and X10&X350 bins, respectively.
  #crpbinData$nearAnti <- (crpbinData$X170+crpbinData$X190)/2
  #crpbinData$nearIn <- (crpbinData$X10+crpbinData$X350)/2
  
  
### Do some modeling! ---------------------------
  # The following use the WIDE format data, looking at individual bins. 
  m000 <- lmer(X190 ~ 1 + (1|ID), data = crpbinData)
  summary(m000)
  anova(m000)

  m001 <- lmer(X190 ~ 1 + ST + (1|ID), data = crpbinData)
  summary(m001)
  anova(m001)
  
  m002 <- lmer(X190 ~ 1 + Speed + (1|ID), data = crpbinData)
  summary(m002)
  anova(m002)
  
  m003 <- lmer(X190 ~ 1 + Speed + ST + (1|ID), data = crpbinData)
  summary(m003)
  anova(m003)
  
  m004 <- lmer(X190 ~ 1 + Pendulums + (1|ID), data = crpbinData)
  summary(m004)
  anova(m004)
  
  m005 <- lmer(X190 ~ 1 + Speed + Pendulums + ST + (1|ID), data = crpbinData)
  summary(m005)
  anova(m005)

  # SPEED and ST work on 190 bin
  
  m100 <- lmer(nearAnti ~ 1 + (1|ID), data = crpbinData)
  summary(m100)
  anova(m100)
  
  m101 <- lmer(nearAnti ~ 1 + ST + (1|ID), data = crpbinData)
  summary(m101)
  anova(m101)
  
  m102 <- lmer(nearAnti ~ 1 + Speed + (1|ID), data = crpbinData)
  summary(m102)
  anova(m102)
  
  m103 <- lmer(nearAnti ~ 1 + Pendulums + (1|ID), data = crpbinData)
  summary(m103)
  anova(m103)
  
  m104 <- lmer(nearAnti ~ 1 + Speed + ST + (1|ID), data = crpbinData)
  summary(m104)
  anova(m104)
  
  m105 <- lmer(nearAnti ~ 1 + Speed + Pendulums + ST + (1|ID), data = crpbinData)
  summary(m105)
  anova(m105)
  
  # 
  
  m200 <- lmer(nearIn ~ 1 + (1|ID), data = crpbinData)
  summary(m200)
  anova(m200)
  
  m201 <- lmer(nearIn ~ 1 + ST + (1|ID), data = crpbinData)
  summary(m201)
  anova(m201)
  
  m202 <- lmer(nearIn ~ 1 + Speed + (1|ID), data = crpbinData)
  summary(m202)
  anova(m202)
  
  m203 <- lmer(nearIn ~ 1 + Pendulums + (1|ID), data = crpbinData)
  summary(m203)
  anova(m203)
  
  m204 <- lmer(nearIn ~ 1 + Speed + ST + (1|ID), data = crpbinData)
  summary(m204)
  anova(m204)
  
  m205 <- lmer(nearIn ~ 1 + Speed + Pendulums + ST + (1|ID), data = crpbinData)
  summary(m205)
  anova(m205)
  
  # 

### Do modeling on LONG format data -----------------------
  mL000 <- lmer(TimeSpent ~ 1 + (1|ID), data = crpbinDataLong)
  summary(mL000)
  anova(mL000)
  
  mL001 <- lmer(TimeSpent ~ 1 + ST + (1|ID), data = crpbinDataLong)
  summary(mL001)
  anova(mL001)
  
  mL002 <- lmer(TimeSpent ~ 1 + Speed + (1|ID), data = crpbinDataLong)
  summary(mL002)
  anova(mL002)
  
  mL003 <- lmer(TimeSpent ~ 1 + Pendulums + (1|ID), data = crpbinDataLong)
  summary(mL003)
  anova(mL003)
  
  # None of the above work, which is good, since that should be impossible. 
  
  mL004 <- lmer(TimeSpent ~ 1 + Bin + (1|ID), data = crpbinDataLong)
  summary(mL004)
  anova(mL004)
  
  mL005 <- lmer(TimeSpent ~ 1 + Bin + ST + (1|ID), data = crpbinDataLong)
  summary(mL005)
  anova(mL005)
  
  mL006 <- lmer(TimeSpent ~ 1 + Bin + ST + Bin*ST + (1|ID), data = crpbinDataLong)
  summary(mL006)
  anova(mL006)
  # Bin * ST interaction is super significant, corroborates EXP 1 results. 
  
  mL007 <- lmer(TimeSpent ~ 1 + Bin + ST + Bin*ST + (1|ID) + (1|Trial:ID), data = crpbinDataLong)
  summary(mL007)
  anova(mL007)
  r.squaredGLMM(mL007)
  # Still good, these are the ones to use. Lots of individual bin * st interactions are sig.
  
  mL008 <- lmer(TimeSpent ~ 1 + Bin + Pendulums + Bin*Pendulums + (1|ID) + (1|Trial:ID), data = crpbinDataLong)
  summary(mL009)
  anova(mL009)
  
  mL010 <- lmer(TimeSpent ~ 1 + Bin + Pendulums + ST + Bin*Pendulums*ST + (1|ID) + (1|Trial:ID), data = crpbinDataLong)
  summary(mL010)
  anova(mL010)
  # Sig Bin*Pendulums*ST interaction
  
  mL011 <- lmer(TimeSpent ~ 1 + Bin + Speed + ST + Bin*Speed*ST + (1|ID) + (1|Trial:ID), data = crpbinDataLong)
  summary(mL011)
  anova(mL011)
  # Sig Bin*Speed*ST interaction
  
  mL012 <- lmer(TimeSpent ~ 1 + Bin + Speed + Pendulums + ST + Bin*Speed*Pendulums*ST + (1|ID) + (1|Trial:ID), data = crpbinDataLong)
  summary(mL012)
  anova(mL012)
  # Four way Bin*Speed*Pendulums*ST doesn't work, but all two and three way interactions involved Bin do work. 
  r.squaredGLMM(mL012)
  # r^2c = .517
  
  mL013 <- lmer(TimeSpent ~ 1 + Bin + Speed + Pendulums + Bin*Speed*Pendulums + (1|ID) + (1|Trial:ID), data = crpbinDataLong)
  summary(mL013)
  anova(mL013)
  r.squaredGLMM(mL013)
  
  anova(mL012, mL013)
  # mL012 (the one that includes ST) is sig better than the one that doesn't.
  
  
### Do some descriptives & graphing ------------------------------------
  
  # Create a vector containing means for each of the 18 bins
  crpbinMeans <- vector(mode="numeric", length=18)
  crpbinMeans[1] <- mean(crpbinData$X10)
  crpbinMeans[2] <- mean(crpbinData$X30)
  crpbinMeans[3] <- mean(crpbinData$X50)
  crpbinMeans[4] <- mean(crpbinData$X70)
  crpbinMeans[5] <- mean(crpbinData$X90)
  crpbinMeans[6] <- mean(crpbinData$X110)
  crpbinMeans[7] <- mean(crpbinData$X130)
  crpbinMeans[8] <- mean(crpbinData$X150)
  crpbinMeans[9] <- mean(crpbinData$X170)
  crpbinMeans[10] <- mean(crpbinData$X190)
  crpbinMeans[11] <- mean(crpbinData$X210)
  crpbinMeans[12] <- mean(crpbinData$X230)
  crpbinMeans[13] <- mean(crpbinData$X250)
  crpbinMeans[14] <- mean(crpbinData$X270)
  crpbinMeans[15] <- mean(crpbinData$X290)
  crpbinMeans[16] <- mean(crpbinData$X310)
  crpbinMeans[17] <- mean(crpbinData$X330)
  crpbinMeans[18] <- mean(crpbinData$X350)
  
  # Plot bin means
  plot(crpbinMeans)
  
  # Create subgroups based on ST conditions
  groupNoST <- subset(crpbinData, ST=="1")
  groupYesST <- subset(crpbinData, ST=="2")
  
  # Create two vectors containing bin means. One vector for each ST condition
  crpbinMeansNoST <- vector(mode="numeric", length=18)
  crpbinMeansNoST[1] <- mean(groupNoST$X10)
  crpbinMeansNoST[2] <- mean(groupNoST$X30)
  crpbinMeansNoST[3] <- mean(groupNoST$X50)
  crpbinMeansNoST[4] <- mean(groupNoST$X70)
  crpbinMeansNoST[5] <- mean(groupNoST$X90)
  crpbinMeansNoST[6] <- mean(groupNoST$X110)
  crpbinMeansNoST[7] <- mean(groupNoST$X130)
  crpbinMeansNoST[8] <- mean(groupNoST$X150)
  crpbinMeansNoST[9] <- mean(groupNoST$X170)
  crpbinMeansNoST[10] <- mean(groupNoST$X190)
  crpbinMeansNoST[11] <- mean(groupNoST$X210)
  crpbinMeansNoST[12] <- mean(groupNoST$X230)
  crpbinMeansNoST[13] <- mean(groupNoST$X250)
  crpbinMeansNoST[14] <- mean(groupNoST$X270)
  crpbinMeansNoST[15] <- mean(groupNoST$X290)
  crpbinMeansNoST[16] <- mean(groupNoST$X310)
  crpbinMeansNoST[17] <- mean(groupNoST$X330)
  crpbinMeansNoST[18] <- mean(groupNoST$X350)
  
  crpbinMeansYesST <- vector(mode="numeric", length=18)
  crpbinMeansYesST[1] <- mean(groupYesST$X10)
  crpbinMeansYesST[2] <- mean(groupYesST$X30)
  crpbinMeansYesST[3] <- mean(groupYesST$X50)
  crpbinMeansYesST[4] <- mean(groupYesST$X70)
  crpbinMeansYesST[5] <- mean(groupYesST$X90)
  crpbinMeansYesST[6] <- mean(groupYesST$X110)
  crpbinMeansYesST[7] <- mean(groupYesST$X130)
  crpbinMeansYesST[8] <- mean(groupYesST$X150)
  crpbinMeansYesST[9] <- mean(groupYesST$X170)
  crpbinMeansYesST[10] <- mean(groupYesST$X190)
  crpbinMeansYesST[11] <- mean(groupYesST$X210)
  crpbinMeansYesST[12] <- mean(groupYesST$X230)
  crpbinMeansYesST[13] <- mean(groupYesST$X250)
  crpbinMeansYesST[14] <- mean(groupYesST$X270)
  crpbinMeansYesST[15] <- mean(groupYesST$X290)
  crpbinMeansYesST[16] <- mean(groupYesST$X310)
  crpbinMeansYesST[17] <- mean(groupYesST$X330)
  crpbinMeansYesST[18] <- mean(groupYesST$X350)
  
  # Plot the two vectors of means on top of each other
  plot(crpbinMeansNoST, xlab="Bin", ylab="%time")
  points(crpbinMeansYesST, col="red")
  
  
  
  # LONG data plotting
  plot(crpbinDataLong$Bin, crpbinDataLong$TimeSpent, col = ifelse(crpbinDataLong$ST==1,"red", "blue"))
  
  

  # Plot NoST and ST
  longST0 <- subset(crpbinDataLong, ST=="1")
  longST1 <- subset(crpbinDataLong, ST=="2")
  
  plot(longST0$Bin, longST0$TimeSpent, ylim = c(0, 60), col = "blue", main = "Control", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  plot(longST1$Bin, longST1$TimeSpent, ylim = c(0, 60), col = "red", main = "ST", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  
  longST0Speed120 <- subset(longST0, Speed=="120")
  longST0Speed180 <- subset(longST0, Speed=="180")
  longST0Speed240 <- subset(longST0, Speed=="240")
  longST1Speed120 <- subset(longST1, Speed=="120")
  longST1Speed180 <- subset(longST1, Speed=="180")
  longST1Speed240 <- subset(longST1, Speed=="240")
  
  # Individual plots for ST and Tempo conditions
  plot(longST0Speed120$Bin, longST0Speed120$TimeSpent, ylim = c(0, 60), col = "blue", main = "Control at 1hz", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  plot(longST0Speed180$Bin, longST0Speed180$TimeSpent, ylim = c(0, 60), col = "blue", main = "Control at 1.5hz", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  plot(longST0Speed240$Bin, longST0Speed240$TimeSpent, ylim = c(0, 60), col = "blue", main = "Control at 2hz", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  
  plot(longST1Speed120$Bin, longST1Speed120$TimeSpent, ylim = c(0, 60), col = "red", main = "ST at 1hz", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  plot(longST1Speed180$Bin, longST1Speed180$TimeSpent, ylim = c(0, 60), col = "red", main = "ST at 1.5hz", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  plot(longST1Speed240$Bin, longST1Speed240$TimeSpent, ylim = c(0, 60), col = "red", main = "ST at 2hz", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  
  
  longST0PendL <- subset(longST0, Pendulums=="L")
  longST0PendM <- subset(longST0, Pendulums=="M")
  longST0PendR <- subset(longST0, Pendulums=="R")
  longST1PendL <- subset(longST1, Pendulums=="L")
  longST1PendM <- subset(longST1, Pendulums=="M")
  longST1PendR <- subset(longST1, Pendulums=="R")
  
  plot(longST0PendL$Bin, longST0PendL$TimeSpent, ylim = c(0, 60), col = "blue", main = "Control with LL Pendulums", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  plot(longST0PendM$Bin, longST0PendM$TimeSpent, ylim = c(0, 60), col = "blue", main = "Control with M Pendulums", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  plot(longST0PendR$Bin, longST0PendR$TimeSpent, ylim = c(0, 60), col = "blue", main = "Control with RL Pendulums", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  
  plot(longST1PendL$Bin, longST1PendL$TimeSpent, ylim = c(0, 60), col = "red", main = "ST with LL Pendulums", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  plot(longST1PendM$Bin, longST1PendM$TimeSpent, ylim = c(0, 60), col = "red", main = "ST with M Pendulums", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  plot(longST1PendR$Bin, longST1PendR$TimeSpent, ylim = c(0, 60), col = "red", main = "ST with RL Pendulums", ylab = "Time Spent in RelPhase Region", xlab = "Bin")
  
  
  
  
### Post-hocs and simple main effects -----------------
  
  subX170 <- subset(crpbinDataLong, Bin=="X170")
  subX190 <- subset(crpbinDataLong, Bin=="X190")
  subX190ST0 <- subset(subX190, ST=="2")
  subX190ST1 <- subset(subX190, ST=="3")
  
  sm001 <- lmer(TimeSpent ~ 1 + ST + (1|ID), data = subX170)
  summary(sm001)
  anova(sm001)
  
  sm002 <- lmer(TimeSpent ~ 1 + ST + (1|ID), data = subX190)
  summary(sm002)
  anova(sm002)