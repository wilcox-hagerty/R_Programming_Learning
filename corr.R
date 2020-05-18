corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the csv files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all variables)
        ## required to compute the correlation between nitrate and sulfate
        ## the default is 0.
        
        ## Return a numeric vector of correlations
        ## Do not round the result. 
        
        ## --------------------------------
        
        ## get list of all csv files in directory
        csv_files <- list.files(directory, full.names = TRUE)
        
        
        ## initiallize empty numeric vector. 
        ## Don't name columns - You can do that after data is filled.
        
        c <- vector('numeric', length = 0)
        
        ## for loop for each file id (in entire directory)
        for(i in 1:length(csv_files)) {
                
                ## read the csv file
                x <- read.csv(csv_files[i])
                ## count number of complete cases - store as ccx
                ccx <- sum(complete.cases(x))
                
                if (ccx >= threshold) {
                        ## if number of complete cases above threshold...
                
                        ## create a dataframe of x WHICH sulfate is not NA 
                        ## WHICH gives row numbers where the logic is TRUE
                        ## filter x by the WHICH statement
                        
                        xSulfate <- x[which(!is.na(x$sulfate)), ]
## see if you can combine into one WHICH statement with &                        
                        
                        ## filter again to remove any rows that have NA in nitrate column.
                        ## Use a WHICH statement for pollutant on xSulfate
                        ## call this 'xPollutant' becuase it has all the pollutant data we want
                        
                        xPollutant <- xSulfate[which(!is.na(xSulfate$nitrate)), ]
                        
                        
                        ## update the c vector with the correlation 
                        ## between sulfate and nitrate columns
                        
                        c <- c(c, cor(xPollutant$sulfate, xPollutant$nitrate))
                        
                }
        }

        return(c)        
}



## rewrite using the "complete.R" function. WHERE nobs > threshold??
