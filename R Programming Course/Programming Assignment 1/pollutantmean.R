#'  A function to read in air pollution samples
#'  from disk, and compute the mean vaues for 
#'  nitrate or sulfate samples across all desired sensors. 
#'  This function will ignore NA values if encountered.
#'  
#'  @param directory the directory containing CSV files of air sample data.
#'  @param pollutant the pollutant to calculate the mean for
#'  @param id the sensor identifiers you want to get complete sample counts for.
#'  @return The mean of the requested pollutant for the sensors requested.
pollutantmean <- function(directory, pollutant, id=1:332)
{
  filePattern ="*.csv"
  files <- list.files(pattern=filePattern, path=directory)
  
  totalSum = 0
  numObs = 0
  for(f in files){
    data = read.csv(paste(directory,"/",f,sep = ""))
    rows = subset(data[[pollutant]], subset =data$ID %in% id)
    fileSum =sum(rows,na.rm = TRUE)
    numRows = length(na.omit(rows))
    
    totalSum = totalSum + fileSum
    numObs =numObs + numRows
  }
  
  totalSum / numObs
}