add2 <- function(x, y) {
  x + y
}

above10 <- function(x){
  use <- x > 10
  x[use]
}

above <- function(x, n = 10) {
  use <- x > n
  x[use]
}

columnmean <- function(y, removeNA = TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[, i, na.rm = removeNA])
  }
  means
}

icount <- function(x) {
  count <- 0
  current <- ""
  curnew <- ""
  for(i in 1:length(x)) {
    current <- as.character(x[i])
    curnew <- ""
    for(k in nchar(current):1) {
      curnew <- paste(curnew, substr(current, k, k), sep = "")
    }
    for(j in 1:length(x)) {
      if(curnew == as.character(x[j])) {
        count <- count + 1
      }
    }
  }
  count
}