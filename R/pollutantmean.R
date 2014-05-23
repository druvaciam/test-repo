pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  date <- c()
  for(i in id) {
    te <- formatC(i, width=3, flag="0")
    te1 = paste(te, "csv", sep = ".")
    filename = paste(directory, te1, sep = "/")
    datebyID <- read.csv(filename, header = T)
    date <- c(date, datebyID[[pollutant]])
  }
  good <- !is.na(date)
  cleandate <- date[good]
  mean(cleandate)
}