#Whilst there are probably better ways around this, such as finding an existing path_finder function within R, or changing Corrie's path_finder in .js to accept same data as R, I wanted some practise with csv manipulation in R :~)

library(tidyverse)
setwd("/Users/Tom/Desktop/R network")

input <- read_csv("data csvs/mini_test_data.csv", col_names = FALSE, skip_empty_rows = TRUE) #Import master csv, formatted for pathfinder
output <- data.frame(source = NULL, destination = NULL, married = NULL, year = NULL) #Create empty df for output... needs column names for rbind() to work... remove at end?... can write csv without header

outputRowCount <- 0

for (i in 1:nrow(input)) {
  
  #Convert input row i from tbl_df to character class
  inputRowi <- input %>%
    slice(i) %>%
    unlist(.,)
  
  #Get positions of "@" and year in input row i
  atPos <- match("@", inputRowi)
  yearPos <- which(!is.na(as.numeric(inputRowi)))
  
  #Create list of parents in input row i
  rowParents <- inputRowi[1:(atPos-1)] #... need to alphabetise parents for hubName
  
  #Create list of children in input row i, accounting for case with zero children
  if (yearPos-atPos == 1) {
    rowChildren <- NA
  } else {
    rowChildren <- inputRowi[(atPos+1):(yearPos-1)]
  }
  
  #Create name of row hub. NB each row of input csv should define a unique hub
  hubName <- paste("hub,", toString(rowParents))
  
  #Create boolean for parents of input row i being married
  rowMarried <- NULL
  if (!is.na(inputRowi[yearPos+1])) {
    rowMarried <- TRUE
  } else {
    rowMarried <- FALSE
  }
  
  #Create variable for year of input row i
  rowYear <- inputRowi[yearPos]
  
  #Create rows in output for parent-to-hub edges
  for (j1 in 1:length(rowParents)) {
    newOutputRow <- data.frame(source = NULL, destination = NULL, married = NULL, year = NULL)
    newOutputRow[1,1] <- rowParents[j1]
    newOutputRow[1,2] <- hubName
    newOutputRow[1,3] <- NA
    newOutputRow[1,4] <- rowYear
    
    output <- rbind(output, newOutputRow)
    outputRowCount <- outputRowCount + 1
  }
  
  #Create rows in output for hub-to-child edges. NB zero children case required here; blocks above and below do not require zero case, since zero parents is improbable.
  for (j2 in 1:length(rowChildren)) {
    if (is.na(rowChildren)) {
      break
    }
    newOutputRow <- data.frame(source = NULL, destination = NULL, married = NULL, year = NULL)
    newOutputRow[1,1] <- hubName
    newOutputRow[1,2] <- rowChildren[j2]
    newOutputRow[1,3] <- NA
    newOutputRow[1,4] <- rowYear
    
    output <- rbind(output, newOutputRow)
    outputRowCount <- outputRowCount + 1
  }
  
  #Create rows in output for parent-to-parent edges.
  numParents <- length(rowParents)
  
  for (j3 in 1:(numParents-1)) {
    k = 1
    while (k < numParents) {
      newOutputRow <- data.frame(source = NULL, destination = NULL, married = NULL, year = NULL)
      newOutputRow[1,1] <- rowParents[j3]
      newOutputRow[1,2] <- rowParents[j3+k]
      if (!is.na(inputRowi[yearPos+1])) { #Distinguish married couples from unmarried couples
        newOutputRow[1,3] <- "y"
      } else {
        newOutputRow[1,3] <- NA
      }
      newOutputRow[1,4] <- rowYear
      
      output <- rbind(output, newOutputRow)
      outputRowCount = outputRowCount + 1
      k = k+1
      if (j3+k > numParents) {
        break
      }
    }
  }
  
}

colnames(output) <- c("source", "destination", "married", "year") #... unsure why these didn't stick from earlier
write.csv(output, file = "/Users/Tom/Desktop/R network/data csvs/isthisfamily.csv", row.names = FALSE, na = "")

