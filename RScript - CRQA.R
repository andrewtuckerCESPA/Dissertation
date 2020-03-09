---
  title: "Andy's CRQA for Dissertation Experiment 3"
  author:"Andy Tucker"
---

### Load & setup data --------------------------
# Libraries for fun & Proffitt
  library(ggplot2)
  library(lme4)
  library(shiny)
  library(crqa)
  
# Load one sample trial to test   
  setwd("C:/Users/Andy/Documents/Projects/Dissertation - HKB-ST/Data/Exp 3/EXP3 - All Participants - ALL Trials")
  sampleTrial <- read.csv("D201 - T04 - 120M.txt")
  col_headings <- c('TSrh','TSlh')
  names(sampleTrial) <- col_headings
  
#Flips sign of left hand time series to account for data collection silliness & make the two time series properly antiphase
  sampleTrial$TSlh <- sampleTrial$TSlh * -1

# Do a plot of the time series
  ggplot(data = sampleTrial[1000:3000,]) + geom_line(aes(y=TSrh, x = 1:2001), color = "blue", size = 1) + geom_line(aes(y = TSlh, x = 1:2001), color = "red", size = 1) + xlab("Time") + ylab("Position") + ggtitle("RH (Blue) and LH (Red) Time Series")
  

### Optimize parameters (radius, delay, embedding) to calculate CRQ -----------------------
## for time-series of continuous variables.  
## use function optimizeParam
  par = list(lgM =  20,
             steps = seq(1, 6, 1),  ## lookahead points to define a local minimum
             cut.del = seq(1, 40,1),## the sequence of delays to be evaluated
             radiusspan = 30,       ## the span of radius examined, relative to
             ## SD of the distance matrix 
             ## (larger constant -> wider range of values)
             radiussample = 10, ## the number of samples of equally
             ## spaced radius values to be examined
             normalize = 1, rescale = 1, mindiagline = 2, minvertline = 2,
             tw = 0, whiteline = FALSE, recpt = FALSE,
             fnnpercent = 10, typeami = "mindip"
             ## other param necessary to compute crqa (see help(crqa))
  ) 
  
  #ts1 <- as.integer(sampleTrial$TSrh[1000:(NROW(sampleTrial)-500)])
  #ts2 <- as.integer(sampleTrial$TSlh[1000:(NROW(sampleTrial)-500)])
  ts1 <- as.integer(sampleTrial$TSrh[1000:8000])
  ts2 <- as.integer(sampleTrial$TSlh[1000:8000])
  
  tm = system.time(
    for (i in 1){
      ansOptim = optimizeParam(ts1, ts2, par)
    }
  )
  
  print(tm)
  print(ansOptim)
  
## it returns the radius, embedding dimension and delay
## to optimize recurrence
  
  
# Set parameters for CRQA 
  delay = ansOptim$delay; 
  embed = ansOptim$emddim; 
  rescale = 1; 
  radius = ansOptim$radius;
  normalize = 1; 
  mindiagline = 2; 
  minvertline = 2;
  tw = 0; 
  whiteline = FALSE; 
  recpt = FALSE; 
  side = "both"
  checkl = list(do = FALSE, thrshd = 3, datatype = "numerical", pad = TRUE)
  
  ts1long <- as.integer(sampleTrial$TSrh[1000:(NROW(sampleTrial)-500)])
  ts2long <- as.integer(sampleTrial$TSlh[1000:(NROW(sampleTrial)-500)])
  
  ts1short <- as.integer(sampleTrial$TSrh[1000:7000])
  ts2short <- as.integer(sampleTrial$TSlh[1000:7000])
  
# Try CRQA + print
  ans = crqa(ts1short, ts2short, delay, embed, rescale, radius, normalize,
       mindiagline, minvertline, tw, whiteline, recpt, side, checkl)
  print(ans)

  
  
  
  
# Plot the Recurrence Plot from ans above
  RP = ans$RP ## take out RP
  RP = matrix(as.numeric(RP), nrow = 2000) ## transform for plotting beauty
  
  wd = 1000
  hg = 1000
  
  ## image(RP) will work just fine
  tstamp = seq(0,2000,200)
  rtime = seq(0, 1, 0.1)
  cols = c("white","blue4")
  
  png("RPlot.png", width = wd, height = hg, res = 600, pointsize = 3)
  
  par(mar = c(2.8, 2.8, 0.2,2), font.axis = 2, cex.axis = 1,
      font.lab = 2, cex.lab = 1.2)
  image(RP, xlab = "", ylab = "",
        xaxt = "n", yaxt = "n", col = cols)
  mtext(tstamp, at = rtime, side = 1, line = .5, cex = 1, font = 2)
  mtext(tstamp, at = rtime, side = 2, line = .5, cex = 1, font = 2,
        adj = 1)
  mtext("RH (TS1)", at = .5, side = 1, line = 1.6, cex = 1, font = 2)
  mtext("LH (TS2)", at = .5, side = 2, line = 1.6, cex = 1, font = 2)
  
  dev.off()

    

###  
### Do all the files in the folder #####
###
  
# Set the file path with the data you want to analyze
  setwd("C:/Users/Andy/Documents/Projects/Dissertation - HKB-ST/Data/Exp 3/EXP3 - All Participants - ALL Trials")
  filesInWd <- list.files(path = getwd(), pattern = "\\.txt$", all.files = FALSE)
  ID <- substr(filesInWd, 2,4)
  Speed <- substr(filesInWd,14,16)
  Pendulums <- substr(filesInWd, 17,17)
  ST <- substr(filesInWd, 2,2)
  output <- data.frame(filesInWd, ID, ST, Speed, Pendulums)
  
  
# Set Param for optimizeParam (to be run below)
  par = list(lgM =  20,
             steps = seq(1, 6, 1),  ## lookahead points to define a local minimum
             cut.del = seq(1, 40,1),## the sequence of delays to be evaluated
             radiusspan = 30,       ## the span of radius examined, relative to
             ## SD of the distance matrix 
             ## (larger constant -> wider range of values)
             radiussample = 10, ## the number of samples of equally
             ## spaced radius values to be examined
             normalize = 1, rescale = 1, mindiagline = 2, minvertline = 2,
             tw = 0, whiteline = FALSE, recpt = FALSE,
             fnnpercent = 10, typeami = "mindip"
             ## other param necessary to compute crqa (see help(crqa))
  ) 
  
# Create list to temporarily hold optimized parameters
  optRadList <- vector(mode="numeric",length=NROW(filesInWd))
  optEmbDimList <- vector(mode="numeric",length=NROW(filesInWd))
  optDelayList <- vector(mode="numeric",length=NROW(filesInWd))
  rowTracker<-1
  
# optimize parameters for all files grabbed above
  for(i in filesInWd){
    cat("Running optimizeParam on: ", i)
    currTrial <- read.csv(i)
    col_headings <- c('tsrh','tslh')
    names(currTrial) <- col_headings
    
    currTrial$tslh <- currTrial$tsrh * -1
    ts1 <- as.integer(currTrial$tsrh[1000:4000])
    ts2 <- as.integer(currTrial$tslh[1000:4000])
    
    tm = system.time(
      for (i in 1){
        ansOptim = optimizeParam(ts1, ts2, par)
      }
    )
    
    print(tm)
    print(ansOptim)
    
    optRadList[rowTracker] <- ansOptim$radius
    optEmbDimList[rowTracker] <- ansOptim$emddim
    optDelayList[rowTracker] <- ansOptim$delay
    rowTracker <- rowTracker + 1
  }

# Filling in output with typical parameters from optimParam, to save time by not running optimParam on all files
  for(i in filesInWd){
    optRadList[rowTracker] <- 39
    optEmbDimList[rowTracker] <- 3
    optDelayList[rowTracker] <- 19
    rowTracker <- rowTracker + 1
  }
  
# Add optimized parameters to output  
  output$optimRadius <- optRadList
  output$optimEmbDim <- optEmbDimList
  output$optimDelay <- optDelayList
    
# Set paramaters for CRQA
  rescale = 1; 
  normalize = 1; 
  indiagline = 2; 
  mindiagline = 2;
  minvertline = 2;
  tw = 0; 
  whiteline = FALSE; 
  recpt = FALSE; 
  side = "both"
  checkl = list(do = FALSE, thrshd = 3, datatype = "numerical", pad = TRUE)
  
  rowTracker<-1
  rrList <- vector(mode="numeric",length=NROW(filesInWd))
  detList <- vector(mode="numeric",length=NROW(filesInWd))
  nrLineList <- vector(mode="numeric",length=NROW(filesInWd))
  maxLineList <- vector(mode="numeric",length=NROW(filesInWd))
  lList <- vector(mode="numeric",length=NROW(filesInWd))
  entrList <- vector(mode="numeric",length=NROW(filesInWd))
  lamList <- vector(mode="numeric",length=NROW(filesInWd))
  ttList <- vector(mode="numeric",length=NROW(filesInWd))
  rpList <- vector(mode="numeric",length=NROW(filesInWd))
  
# Run CRQA for each trial
  for(i in filesInWd){
    cat("Running CRQA on: ", i)
    currTrial <- read.csv(filesInWd[rowTracker])
    col_headings <- c('tsrh','tslh')
    names(currTrial) <- col_headings
    
    #Use optimized parameters from above
    delay = output$optimRadius[rowTracker]; 
    embed = output$optimEmbDim[rowTracker]; 
    radius = output$optimDelay[rowTracker];
    
    #Chops first 10 second and last 5 seconds from each trial
    #ts1long <- as.integer(currTrial$tsrh[1000:(NROW(currTrial)-500)])
    #ts2long <- as.integer(currTrial$tslh[1000:(NROW(currTrial)-500)])
    ts1 <- as.integer(currTrial$tsrh[1000:5000])
    ts2 <- as.integer(currTrial$tslh[1000:5000])
    
    # Try CRQA + print
    ans = crqa(ts1, ts2, delay, embed, rescale, radius, normalize,
               mindiagline, minvertline, tw, whiteline, recpt, side, checkl)
    print(ans)
    
    # Add results to lists and icnrement count
    rrList[rowTracker] <- ans$RR
    detList[rowTracker] <- ans$DET
    nrLineList[rowTracker] <- ans$NRLINE
    maxLineList[rowTracker] <- ans$maxL
    lList[rowTracker] <- ans$L
    entrList[rowTracker] <- ans$ENTR
    lamList[rowTracker] <- ans$LAM
    ttList[rowTracker] <- ans$TT
    rpList[rowTracker] <- ans$RP
    rowTracker <- rowTracker + 1
  }
  
  output$rr <- rrList
  output$det <- detList
  output$nrLine <- nrLineList
  output$maxLine <- maxLineList
  output$l <- lList
  output$entr <- entrList
  output$lam <- lamList
  output$tt <- ttList
  output$rp <- rpList
  
  write.csv(output, 'CRQAoutput.csv')
  
  
  ###  
  # END # 
  ###
  