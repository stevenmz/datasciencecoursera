best <- function(state, outcome) {
  ## Read outcome data
  data = read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE )
  
  ## Check that state and outcome are valid
  if((state %in% data$State) == FALSE){
    stop("invalid state")
  }
  
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  if((outcome %in% names(outcomes)) == FALSE){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  subdata = subset(data, data$State == state)[, c(2,7,outcomes[outcome])]
  
  subdata = na.omit(subdata)
  subdata = subdata[order(subdata[,3]),] #sort by column code: http://stackoverflow.com/a/1296745/534347
  
  ## rate
  subdata[1,1]
}