complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  ## Return a data frame of the form:
  ## id nobs
  ## 1 117
  ## 2 1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  # get the length of id vector
  id_len <- length(id)
  complete_data <- rep(0, id_len)
  # find all files in the specdata folder
  files_full <- list.files(directory,full.names=TRUE)
  #file_paths <- paste(directory, all_files, sep="")
  j <- 1
  for (i in id) {
    current_file <- read.csv(files_full[i], header=T)
    complete_data[j] <- sum(complete.cases(current_file))
    j <- j + 1
  }
  result <- data.frame(id = id, nobs = complete_data)
  return(result)
} 