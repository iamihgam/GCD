# 1. Merge the training and the test sets to create one data set.

# Set working directory
setwd("C:/Users/Lezhnina/Desktop/Coursera/GCD/UCI HAR Dataset")

# Read data from files
features     = read.table('./features.txt',header=F)
activityType = read.table('./activity_labels.txt',header=F)
subjectTrain = read.table('./train/subject_train.txt',header=F)
xTrain       = read.table('./train/x_train.txt',header=F)
yTrain       = read.table('./train/y_train.txt',header=F)

#  Create new column names
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "activityId"

# Create the final training set 
trainingData = cbind(yTrain,subjectTrain,xTrain)

# Read in the test data
subjectTest = read.table('./test/subject_test.txt',header=F)
xTest       = read.table('./test/x_test.txt',header=F)
yTest       = read.table('./test/y_test.txt',header=F)

# Assign column names to the test data
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2] 
colnames(yTest)       = "activityId"


# Create the final test set 
testData = cbind(yTest,subjectTest,xTest)


# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData)

# Create a vector for the column names from the finalData
colNames  = colnames(finalData)



# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Subset finalData
finalData = finalData[logicalVector==T]




# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=T)

# Updating the colNames vector
colNames  = colnames(finalData)




# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

colnames(finalData) = colNames





# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType']

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=T)

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=T,sep='\t')
