#' @title Coursera's Getting and Cleaning Data - Class project
#' @author Steven Magana-Zook
#' Created: April 1, 2016


# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Part 1.
# Load the test and training sets along with their labels.
# Also load the subject identifier for each measurement.
# try to speed up reading large files using R Peng's suggestions

# Read in the feature names so we can use them as column names in the dataframes
# Supply nrows, colClasses to make large dtafrme reading more efficient
# wc -l features.txt: 561 features.txt so I will overshoot with 570
columnNames = read.table(
  "features.txt",
  colClasses = "character",
  col.names = c("index","featurename"),
  nrows = 570)

#read in the training dataset
# Supply nrows, colClasses to make large dtafrme reading more efficient
# wc -l X_train.txt: 7352 X_train.txt so I will overshoot with 7400
xtrain = read.table(
  "train/X_train.txt",
  colClasses = "numeric",
  col.names = columnNames$featurename,
  nrows=7400)

# Read in the subject identifiers for each measurement
xtrain$id = read.table(
  "train/subject_train.txt",
  colClasses = "numeric",
  col.names = c("id"),
  nrows=7400)[[1]]

# Add the training labels to the dataframe
# Supply nrows, colClasses to make large dtafrme reading more efficient
xtrain$labels = read.table(
  "train/y_train.txt",
  colClasses = "numeric",
  nrows=7400)[[1]]

# do the same reading in and apply labels for the test data
# Supply nrows, colClasses to make large dtafrme reading more efficient
xtest = read.table(
  "test/X_test.txt",
  colClasses = "numeric",
  col.names = columnNames$featurename,
  nrows=3000)

# Add the training labels to the dataframe
xtest$labels = read.table(
  "test/y_test.txt",
  colClasses = "numeric",
  nrows=3000)[[1]]

xtest$id = read.table(
  "test/subject_test.txt",
  colClasses = "numeric",
  col.names = c("id"),
  nrows=3000)[[1]]

# combine the labled and ID'd train and test datasets into one.
data = rbind(xtrain,xtest)

# Clean up the environment to save memory
rm(xtest,xtrain,columnNames)

## END OF PART 1 ##

# Part 2: Extracts only the measurements on the mean and standard deviation for each measurement.

# Find all of the columns containing mean() and std()
# Bring along the ID and Label columns as well
meanStdColumns = grep("mean()|std()|id|label",names(data),value=TRUE)
tidyData = data[,meanStdColumns]

# clean up the environment to save memory
rm(data, meanStdColumns)

## END OF PART 2 ##

# Part 3:  Uses descriptive activity names to name the activities in the data set
# Labels:
#   1 WALKING
#   2 WALKING_UPSTAIRS
#   3 WALKING_DOWNSTAIRS
#   4 SITTING
#   5 STANDING
#   6 LAYING
tidyData$labels[tidyData$labels == 1] = "WALKING"
tidyData$labels[tidyData$labels == 2] = "WALKING_UPSTAIRS"
tidyData$labels[tidyData$labels == 3] = "WALKING_DOWNSTAIRS"
tidyData$labels[tidyData$labels == 4] = "SITTING"
tidyData$labels[tidyData$labels == 5] = "STANDING"
tidyData$labels[tidyData$labels == 6] = "LAYING"

## END OF PART 3 ##

# Part 4: Appropriately labels the data set with descriptive variable names.

# !!!
# THIS WAS DONE USING col.names WITH THE columnName VARIABLE WHEN THE DATA FRAMES WERE LOADED
# !!!

## END OF PART 4 ##

# Part 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Group by subject id, then by activity, and calculate the mean for each column.
tidyGroupedData = aggregate(
  # Exclude the id and labels column from being duplicated in final dataset
  subset(tidyData,select=-c(id,labels)),
  # Group by subject id first, then by activity label
  list(IDs = tidyData$id, label = tidyData$labels),
  #Apply the mean function to each grouped column
  mean)

## END OF PART 5 ##

# There is now a tidyData and a tidyGroupedData data frame in the environment
# Write the tidyGroupedData out to a text file
write.table(tidyGroupedData, "tidyData.txt", row.names = FALSE)
