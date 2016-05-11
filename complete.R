complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    
    ## Returns data of the form:
    ## id nobs
    ## 1 117
    ## 2 1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the number of complete cases
    
    original_directory <- getwd()
    setwd(directory)
    
    result <- data.frame(id=numeric(0), nobs=numeric(0))
    cur_result_row <- 1
    
    for (i in id) {
        if (i < 10) {
            filename <- paste("00", i, ".csv", sep="")
        } else if (i < 100) {
            filename <- paste("0", i, ".csv", sep="")
        } else {
            filename <- paste(i, ".csv", sep="")
        }
        data <- read.csv(filename)
        
        s <- sapply(data[,"sulfate"], is.na)
        n <- sapply(data[,"nitrate"], is.na)
        valid_rows <- !s & !n
        num_valid_rows <- sum(valid_rows)
 
        result[cur_result_row, 1] <- i
        result[cur_result_row, 2] <- num_valid_rows
        cur_result_row <- cur_result_row + 1
    }
    
    setwd(original_directory)
    
    result
}