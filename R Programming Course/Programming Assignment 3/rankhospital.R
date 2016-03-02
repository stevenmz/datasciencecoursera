##' A function to find the hospital in a 
##' particular state that has the specified mortality rate for a particular outcome
##' @param state the two-character state abbreviation in all capital letters (i.e. CA for California)
##' @param outcome one of "heart attack", "heart failure", or "pneumonia"
##' @param num either one of these strings "best", "worst" or a numeric index (i.e. 5 for 5th best)
##' @return The name of the hospital matching the parameters you specify or NA for an invalid index provided to the num parameter
##' @author Steven Magana-Zook (smaganazook@live.com, https://github.com/stevenmz)
##' @examples 
##' rankhospital("TX", "heart failure", "best")
##' rankhospital("MD", "heart attack", "worst")
##' rankhospital("CA", "pneumonia", 2)
rankhospital <- function(state, outcome, num = "best") {
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
  
  rowIndex = numeric(1)
  if(class(num) == "character"){
    if(num == "best"){
      rowIndex = 1
    }
    else if(num == "worst"){
      rowIndex = nrow(subdata)
    }
    else{
      stop("Invalid character string passed to num parameter! Valid values are \"best\" or \"worst\".")
    }
  }else if(class(num) == "numeric"){
    rowIndex = num
  }
  
  #validate row index, and if valid then return the hospital name
  if(rowIndex > nrow(subdata)){
    NA
  }
  else {
    ## Return the hospital with the specified mortality rate for the specified outcome
    subdata[rowIndex,1]
  }
  
}