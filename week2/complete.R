complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the csv files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used. 
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases.
 
        ## -----------------------------------------------
        
        ## get list of all csv files in directory
        csv_files <- list.files(directory, full.names = TRUE)
        
        
        ## initiallize empty data frame. 
        ## Don't name columns - You can do that after data is filled.
        
        c <- data.frame()
        
        ## use for loop to grab number of complete lines from each monitor.
        ## for each id, read the file, get sum of complete caces ("nobs").
        
        ## the id of the csv file will be the "id" column
        ## number of complete lines will be the "nobs" column
        
        for(i in id) {
                x <- read.csv(csv_files[i])
                nobs <- sum(complete.cases(x))
                y <- data.frame(i, nobs)
                c <- rbind(c, y)
        }
        
        
        colnames(c) <- c("id", "nobs")
        
        c

}

