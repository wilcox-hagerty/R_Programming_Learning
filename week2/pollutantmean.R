pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the csv files
        
        ## 'pollutant' is a characer vector of length 1 indicating 
        ## the name of the pollutant for which we will calculate the mean
        ## either "sulfate" or "nitrate"
        
        ## 'id' is an integer vector indicating the monitor ID numbers to be used. 
        
        ## Retunr the mean of the pollutant across all monitors listed
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: do not round the result.
        
        
        
        ## create list of all csv files in "id" range
        csv_file_list <- list.files(directory, full.names = TRUE)
        
        ## create empty dataframe that will eventually store all data
        
        pollutant_dataframe <- data.frame()
        
        ## create dataframe with files list.
        ## use for loop to iterate though files and 
                ## add them under the last line on the dataframe 

        for(i in id) {
                x <- read.csv(csv_file_list[i])
                pollutant_dataframe <- rbind(pollutant_dataframe, x)
        }

        mean(pollutant_dataframe[,pollutant],na.rm = TRUE)
}

