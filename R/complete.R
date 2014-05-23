complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  nobs <- c()
  for(i in id) {
    te <- formatC(i, width=3, flag="0")
    te1 = paste(te, "csv", sep = ".")
    filename = paste(directory, te1, sep = "/")
    datebyID <- read.csv(filename, header = T)
    good <- complete.cases(datebyID)
    corcount <- sum(good, na.rm=TRUE)
    nobs <- c(nobs, corcount)
  }
  data.frame(id = id, nobs = nobs)
}