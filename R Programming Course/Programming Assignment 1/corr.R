#'  A function to read in air pollution samples
#'  from disk, and return the correlation between 
#'  nitrate and sulfate samples.
#'  
#'  @param directory the directory containing CSV files of air sample data.
#'  @param threshold the minimum number of complete cases from a sensor to be included in the correlation.
#'  @return A vector of the correlation values.
corr = function(directory, threshold=1){
  data = complete(directory)
  dataToUse = subset(data, subset= data$nobs > threshold)["id"]
  corrs = numeric(0)
  
  for (id in dataToUse[[1]]){
    filename=""
    if(id< 10)
    {
      filename = paste("00",id,".csv",sep="")
    }else if(id< 100)
    {
      filename = paste("0",id,".csv",sep="")
    }
    else
    {
      filename = paste(id,".csv",sep="")
    } 
    
    data = read.csv(paste(directory,"/",filename,sep = ""))
    completerows = subset(data, subset = (!is.na(data$sulfate)  & !is.na(data$nitrate)))
    
    sulfateSamples = completerows["sulfate"]
    nitrateSamples = completerows["nitrate"]
   
    
    corrs = c(corrs, cor(sulfateSamples,nitrateSamples))
  }
  corrs
  
}