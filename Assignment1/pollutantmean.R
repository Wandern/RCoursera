pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  files_full<-list.files(directory,full.names=TRUE)
  files_subset<-files_full[id]
  raw_table<-lapply(file_subset,read.csv,header=TRUE)
  
  concat_table<-do.call(rbind,raw_table)
  
  concat_col<-concat_table[[pollutant]]
  
  conditioned_col<-concat_col[!is.na(concat_col)]
  
  poll_mean <- mean(conditioned_col)
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)



}