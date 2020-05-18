
corr3 <- function(directory, threshold = 0) {
        
        ## get list of all csv files in directory
        csv_files <- list.files(directory, full.names = TRUE)
        
        
        ## initiallize empty numeric vector. 
        ## Don't name columns - You can do that after data is filled.
        
        c <- vector('numeric', length = 0)
        
        ## for loop for each file id (in entire directory)
       
        complete_count <- complete(directory)
        
        above_threshold_id <- complete_count[which(complete_count$nobs >= threshold), ]$id
        
        for (i in above_threshold_id) {
                x <- read.csv(csv_files[i])
                
                c <- c(c, cor(x$sulfate, x$nitrate))
        }
        
        return(c)        
}



## rewrite using the "complete.R" function. WHERE nobs > threshold??
