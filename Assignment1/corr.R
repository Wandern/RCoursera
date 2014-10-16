corr <- function(directory, threshold = 0) {
  
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  ## Return a numeric vector of correlations
  
  # Get the completed table.
  complete_table <- complete(directory, 1:332)
  nobs <- complete_table$nobs
  # Acquire the correct Ids.
  ids <- complete_table$id[nobs > threshold]
  #init an empty vector, to begin adding data into.
  corr_vector <- vector()
  # find all files in the specdata folder
  files_full <- list.files(directory,full.names=TRUE)
  j <- 1
  for(i in ids) {
    #Read data from the current file.
     data<- read.csv(files_full[i], header=T)
     ## Calculate and store the count of complete cases
     completeCases <- data[complete.cases(data),]
     count <- nrow(completeCases)
     ## Calculate and store the count of complete cases
     ## if threshhold is reached
     if( count >= threshold ) {
       corr_vector <- c(corr_vector, cor(completeCases$nitrate, completeCases$sulfate) )
     }
  }
  return(corr_vector)
}
