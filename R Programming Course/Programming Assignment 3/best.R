best <- function(state, outcome) {
  ## Read outcome data
  data = read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE )
  
  ## Check that state and outcome are valid
  if((state %in% data$State) == FALSE){
    stop("invalid state")
  }
  
  # Outcomes code from Al Warren's "[Tips] A few pointers for assignment 3"
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  if((outcome %in% names(outcomes)) == FALSE){
    stop("invalid outcome")
  }
  
  
  # Shrink data to only have the three columns we care about
  # Code from Al Warren's "[Tips] A few pointers for assignment 3"
  subdata = subset(data, data$State == state)[, c(2,7,outcomes[outcome])]
  
  # Shrink data to only those without NA values
  subdata = na.omit(subdata) 
  
  # Sort first by lowest mortality, then by hospital name
  # Sort-by-column code: http://stackoverflow.com/a/1296745/534347
  subdata = subdata[order(subdata[,3],subdata[,1]),] 

  ## Return the hospital with the lowest mortality rate for the specified outcome
  subdata[1,1]
}