rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  outcomebase <- "Hospital.30.Day.Death..Mortality..Rates.from."
  outcolname <- character(0)
  if (outcome == "heart attack") {outcolname <- paste(outcomebase, "Heart.Attack", sep = "")}
  else if (outcome == "heart failure") {outcolname <- paste(outcomebase, "Heart.Failure", sep = "")}
  else if (outcome == "pneumonia") {outcolname <- paste(outcomebase, "Pneumonia", sep = "")}
  else {stop("invalid outcome")}
  
  if (sum(data$State == state) == 0) {stop("invalid state")}
  
  
  
}
