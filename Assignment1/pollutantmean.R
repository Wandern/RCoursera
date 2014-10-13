pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ##Pull in the list of files stored in directory
  files_full <- list.files(directory,full.names=TRUE)
    
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  ##Subset files_full to pull in only the requested ones (id).
  files_subset <- files_full[id]
  
  ##read the data contained within the requested files into a dataframe vector.
  dataframe <- lapply(file_subset,read.csv,header=TRUE)
  
  ##Combine the requested file dataframe vector into a single dataframe
  combined_dataframe <- do.call(rbind,dataframe)
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ##Acquire the column requested by the pollutant input (column name)
  pollutant_column <- combined_dataframe[[pollutant]]
  
  ##Remove NA's from pollutant column
  conditioned_pollutant_column <- pollutant_column[!is.na(pollutant_column)]
  
  ##Perform the mean over the pollutant column once NA's are removed.
  pollutant_mean <- mean(conditioned_pollutant_column)
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  return(pollutant_mean)

}