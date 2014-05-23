corr <- function(directory, threshold = 0) {
  completeres <- complete(directory)
  isabove <- completeres[[2]] > threshold
  if(sum(isabove, na.rm=TRUE) == 0) {
    numeric()
  }
  else {
    aboveIDs <- completeres[[1]][isabove]
    answer <- numeric()
    for(i in aboveIDs) {
      te <- formatC(i, width=3, flag="0")
      te1 = paste(te, "csv", sep = ".")
      filename = paste(directory, te1, sep = "/")
      datebyID <- read.csv(filename, header = T)
      good <- complete.cases(datebyID)
      gooddata <- datebyID[good,]
      cormatrix <- cor(gooddata[c("sulfate", "nitrate")])
      answer <- c(answer, cormatrix[2])
    }
    answer
  }
  
}