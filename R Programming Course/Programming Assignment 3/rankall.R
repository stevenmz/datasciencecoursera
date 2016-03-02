##' A function to find the hospital in every state that has the specified mortality rate for a particular outcome
##' @param outcome one of "heart attack", "heart failure", or "pneumonia"
##' @param num either one of these strings "best", "worst" or a numeric index (i.e. 5 for 5th best)
##' @return 
##' A 2-column data frame containing the name of the hospital for each state matching 
##' the parameters you specify or NA for an invalid index provided to the num parameter,
##' and the state code in the second column.
##' @author Steven Magana-Zook (smaganazook@live.com, https://github.com/stevenmz)
##' @examples 
##' rankall("heart attack", 20)
##' rankall("pneumonia", "worst")
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data = read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE )
  
  # Outcomes code from Al Warren's "[Tips] A few pointers for assignment 3"
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  if((outcome %in% names(outcomes)) == FALSE){
    stop("invalid outcome")
  }
  
  # Shrink data to only have the three columns we care about
  # Code from Al Warren's "[Tips] A few pointers for assignment 3"
  subdata = data[, c(2,7,outcomes[outcome])]
  
  # Shrink data to only those without NA values
  subdata = na.omit(subdata) 
  
  # Sort first by state, then by lowest mortality, and finally by hospital name
  # Sort-by-column code: http://stackoverflow.com/a/1296745/534347
  subdata = subdata[order(subdata[,2],subdata[,3],subdata[,1]),] 
  
  # Create a list of data frames (one for each state). 
  # Each data frame will still be sorted by outcome and hospital name
  statesData = split(subdata, subdata$State)
  
  # Pull the hospital with the specified ranking from each state
  # Inline function is direct copy of rankhospital function (instructed not to call that function)
  hospitalsPerState = lapply(statesData, function(dd) 
  {
    rowIndex = numeric(1)
    if(class(num) == "character"){
      if(num == "best"){
        rowIndex = 1
      }
      else if(num == "worst"){
        rowIndex = nrow(dd
        )
      }
      else{
        stop("Invalid character string passed to num parameter! Valid values are \"best\" or \"worst\".")
      }
    }else if(class(num) == "numeric"){
      rowIndex = num
    }
    
    #validate row index, and if valid then return the hospital name
    if(rowIndex > nrow(dd)){
      NA
    }
    else {
      ## Return the hospital with the specified mortality rate for the specified outcome
      dd[rowIndex,1]
    }
  })
  
  # Return value is a data frame containing the hospital name and state code
  data.frame(Hospital=unlist(hospitalsPerState), 
             State=names(hospitalsPerState), 
             row.names=names(hospitalsPerState), stringsAsFactors = FALSE)
}