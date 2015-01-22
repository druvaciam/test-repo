rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  outcomebase <- "Hospital.30.Day.Death..Mortality..Rates.from."
  outcolname <- character(0)
  if (outcome == "heart attack") {outcolname <- paste(outcomebase, "Heart.Attack", sep = "")}
  else if (outcome == "heart failure") {outcolname <- paste(outcomebase, "Heart.Failure", sep = "")}
  else if (outcome == "pneumonia") {outcolname <- paste(outcomebase, "Pneumonia", sep = "")}
  else {stop("invalid outcome")}
  
  ## For each state, find the hospital of the given rank
  state <- c()
  hospital <- c()
  spl <- split(data, data$State)
  for (s in spl) {
    state <- c(state, s[1, "State"])
    outnumeric <- as.numeric(s[, outcolname])
    goodoutnumeric <- !is.na(outnumeric)
    s <- s[goodoutnumeric, ]
    sorted <- s[order(as.numeric(s[, outcolname]), s[, "Hospital.Name"]), ]
    sortedNames <- sorted[, "Hospital.Name"]
    if (num == "best") {hospital <- c(hospital, sortedNames[1])}
    else if (num == "worst") {hospital <- c(hospital, sortedNames[length(sortedNames)])}
    else if (as.numeric(num) > length(sortedNames)) {hospital <- c(hospital, NA)}
    else {hospital <- c(hospital, sortedNames[as.numeric(num)])}
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital, state)
}
