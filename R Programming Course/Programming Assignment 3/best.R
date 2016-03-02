best <- function(state, outcome) {
  ## Read outcome data
  data = read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE )
  
  ## Check that state and outcome are valid
  if((state %in% data$State) == FALSE){
    stop("invalid state")
  }
  
  if((outcome %in% c("heart attack", "heart failure", "pneumonia")) == FALSE){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  subdata = subset(data, data$state == state)
  View(subdata)
  ## rate
}