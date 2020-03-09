---
  title: "Andy's SCript for Stripping Headers of TXT files from DataLog"
  author:"Andy Tucker"
---
  
  # Set working directory and grab txt file names
  setwd("C:/Users/Andy/Documents/Projects/Dissertation - HKB-ST/Data/Exp 3/EXP3 - All Participants - ALL Trials")
  txtInWD <- list.files(path = getwd(), pattern = "\\.txt$")
  txtFrame <- as.vector(txtInWD)
  names(txtFrame) <- c("FileName")

  
  # Take a peek at the first one
  print(toString(txtFrame[1]))
  
  
  # Testing ability to read in each txt file minus the header, save it back out with alterations
  testFileName <- txtFrame[1]
  testTable <- read.csv(testFileName, skip = 3)
  trimTestTable <- testTable[-nrow(testTable),]
  write.table(trimTestTable, file = "tester.txt", append = FALSE, sep = ",", dec = ".", row.names=FALSE, col.names=FALSE)
  
  
  # Read files and write them out without headers or empty final row
  for(i in txtFrame){
    print(i)
    currTable <- read.csv(i, skip = 3)
    trimCurrTable <- currTable[-nrow(currTable),]
    write.table(trimCurrTable, file = i, append = FALSE, sep = ",", dec = ".", row.names=FALSE, col.names=FALSE)
  }
  
  

  
  