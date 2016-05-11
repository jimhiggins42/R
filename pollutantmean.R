pollutantmean <- function(directory, pollutant, id = 1:332) {
    original_directory <- getwd()
    setwd(directory)
    total_count <- 0
    total_sum <- 0
    for (i in id) {
        if (i < 10) {
            filename <- paste("00", i, ".csv", sep="")
        } else if (i < 100) {
            filename <- paste("0", i, ".csv", sep="")
        } else {
            filename <- paste(i, ".csv", sep="")
        }
        data <- read.csv(filename)

        invalid_rows <- sapply(data[, pollutant], is.na)
        valid_rows <- !invalid_rows
        cur_count <- sum(valid_rows)
        total_count <- total_count + cur_count
        
        cur_sum <- sum(data[, pollutant], na.rm=TRUE)
        total_sum <- total_sum + cur_sum
        
        #print(i)
        #print(cur_sum)
        #print(total_count)
        #print(total_sum)
    }
    mean <- total_sum / total_count

    setwd(original_directory)
        
    mean
}