#'  A function to read in air pollution samples
#'  from disk, and return the samples where both sulfate
#'  and nitrate were reported.
#'  
#'  @param directory the directory containing CSV files of air sample data.
#'  @param id the sensor identifiers you want to get complete sample counts for.
#'  @return A data frame with a variable for the filename and a variable for the count of complete cases.
complete = function(directory, id=1:332)
{
  res = data.frame(id = id, nobs = 0)
  filePattern ="*.csv"
  files <- list.files(pattern=filePattern, path=directory)
  
  
  for(i in id){
    filename=""
    if(i< 10)
    {
      filename = paste("00",i,".csv",sep="")
    }else if(i< 100)
    {
      filename = paste("0",i,".csv",sep="")
    }
    else
    {
      filename = paste(i,".csv",sep="")
    } 
    data = read.csv(paste(directory,"/",filename,sep = ""))
    completerows = subset(data, subset = (!is.na(data$sulfate)  & !is.na(data$nitrate)))
    
    res$nobs[res$id == i] <- nrow(completerows)
  }
  
  res
}