



fileDir <- setwd(getwd())



"pollutantmean" <- function(inputDirectory, inputPollutant, inputID = 1:332)
{
  #returns mean of pollutant across all monitors without rounding 
  allInputs <- {}
  inputIDDF <- data.frame("id" = inputID, "idChar" = inputID)
  inputIDDF$idChar[inputIDDF$id < 10] <- paste0("00", inputIDDF$id[inputIDDF$id < 10])
  inputIDDF$idChar[inputIDDF$id >= 10 & inputIDDF$id < 100] <- paste0("0", inputIDDF$id[inputIDDF$id >= 10 & inputIDDF$id < 100])
  for(i in inputIDDF$idChar){
    thisMonitor <- read.csv(paste0(fileDir, "/", inputDirectory, "/", i, ".csv"), stringsAsFactors = FALSE)
    allInputs <- c(allInputs, thisMonitor[, inputPollutant])
  }
  mean(allInputs, na.rm = TRUE)
}

#inputPollutant is either "sulfate" or "nitrate"
#inputID is an integer vector with ID monitors 


pollutantmean("specdata", "sulfate",  1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "nitrate", 23)


"complete" <- function(inputDirectory, id = 1:332){

  inputIDDF <- data.frame("id" = id, "idChar" = id)
  inputIDDF$idChar[inputIDDF$id < 10] <- paste0("00", inputIDDF$id[inputIDDF$id < 10])
  inputIDDF$idChar[inputIDDF$id >= 10 & inputIDDF$id < 100] <- paste0("0", inputIDDF$id[inputIDDF$id >= 10 & inputIDDF$id < 100])
  
  outputDF <- data.frame("id" = id, nobs = NA, stringsAsFactors = FALSE)
  for(i in outputDF$id){
    thisFile <- inputIDDF$idChar[inputIDDF$id == i][1]
    thisMonitor <- read.csv(paste0(fileDir, "/", inputDirectory, "/", thisFile, ".csv"), stringsAsFactors = FALSE)
    outputDF$nobs[outputDF$id == i] <- nrow(thisMonitor[!is.na(thisMonitor[, "sulfate"]) & !is.na(thisMonitor[, "nitrate"]), ])
  }
  
  return(outputDF)
  
}

complete("specdata", c(2, 4, 8, 10, 12))

complete("specdata", 30:25)

complete("specdata", 3)

"corr" <- function(inputDirectory, threshold = 0){
  
  theseMonitors <- complete("specdata")
  theseMonitors <- theseMonitors[theseMonitors$nobs >= threshold, ]
  
  allInputs <- {}
  
  if(nrow(theseMonitors) > 0){
   inputIDDF <- data.frame("id" = theseMonitors$id, "idChar" = theseMonitors$id)
   inputIDDF$idChar[inputIDDF$id < 10] <- paste0("00", inputIDDF$id[inputIDDF$id < 10])
   inputIDDF$idChar[inputIDDF$id >= 10 & inputIDDF$id < 100] <- paste0("0", inputIDDF$id[inputIDDF$id >= 10 & inputIDDF$id < 100])
  
   for(i in inputIDDF$idChar){
     thisMonitor <- read.csv(paste0(fileDir, "/", inputDirectory, "/", i, ".csv"), stringsAsFactors = FALSE)
     if(nrow(thisMonitor[!is.na(thisMonitor$sulfate) & !is.na(thisMonitor$nitrate), ]) > 1){
      allInputs <- c(allInputs, cor(x = thisMonitor[, c("sulfate", "nitrate")], use = "na.or.complete")[2])
     }
   }
  } 
  allInputs
  
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)

cr <- corr("specdata", 5000)
summary(cr)

cr <- corr("specdata")
summary(cr)
length(cr) #must be 323



pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
