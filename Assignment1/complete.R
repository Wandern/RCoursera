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
  
  #initialize a vector to store the summation of complete data sets per id.
  complete_data <- rep(0, id_len)
  
  # find all files in the specdata folder
  files_full <- list.files(directory,full.names=TRUE)
  #intialize numeric to increment through complete_data (vector storing summation of complete data sets per id.)
  j <- 1
  #for loop through the range of monitors(ids) provided.
  for (i in id) {
    #Acquire the directory for each file in the range of monitors(ids) provided
    current_file <- read.csv(files_full[i], header=T)
    #push the summation of complete cases of data within the current monitor(id) into the vector.
    complete_data[j] <- sum(complete.cases(current_file))
    #increment j so when/if it is used, the next summation of complete cases can be put at the end of the vector.
    j <- j + 1
  }
  #Place the vector of id's alongside their corresponding complete case sums to form a data frame. Add the correct headers to display.
  result <- data.frame(id = id, nobs = complete_data)
  
  #Output the correctly formatted dataframe.
  return(result)
} 