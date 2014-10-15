corr <- function(directory, threshold = 0) {
  
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  ## Return a numeric vector of correlations
  
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  # Get the completed table.
  complete_table <- complete("specdata", 1:332)
  nobs <- complete_table$nobs
  # Acquire the correct Ids.
  ids <- complete_table$id[nobs > threshold]
  # get the length of ids vector
  id_len <- length(ids)
  #init an empty vector of the correct size, to begin adding data into.
  corr_vector <- rep(0, id_len)
  # find all files in the specdata folder
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  #j <- 1
  for(i in ids) {
    #Read data from the current file.
    current_file <- read.csv(file_paths[i], header=T)
    #Add the correlated values into the corr_vector object.
    corr_vector[i] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
    #increment the counter
    #j <- j + 1
  }
  result <- round(corr_vector,5)
  return(result)
}