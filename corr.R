corr <- function(directory, threshold = 0, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the number of completely observed observations
    ## (on all variables) required to compute the correlation between nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    original_directory <- getwd()
    setwd(directory)
    
    #result <- data.frame(id=numeric(0), nobs=numeric(0))
    cur_result_row <- 1
    result <- numeric(0)
    
    for (i in id) {
        #cur_cor <- 0
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
 
        if (num_valid_rows > threshold) {
            cur_cor <- cor(data[, "sulfate"], data[, "nitrate"],use = "complete.obs")
            result[cur_result_row] <- cur_cor
            cur_result_row <- cur_result_row + 1
        }
        
        
        #result[cur_result_row, 1] <- i
        #result[cur_result_row, 2] <- num_valid_rows
    }
    
    setwd(original_directory)
    
    result
}