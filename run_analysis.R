## 1. Merges the training and the test sets to create one data set.

### set workspace
if(!file.exists("./CourseProject")) {dir.create("./CourseProject")}
setwd("./CourseProject")

### load the data from files
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE) ##imports the features
activities <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE) ##imports the names of activities
trainingY <- read.table("./UCI HAR Dataset/train/Y_train.txt", header = FALSE)  ##imports training class label data
trainingX <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)  ##imports training result data
TrainingSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)  ##imports the training subject data
testY <- read.table("./UCI HAR Dataset/test/Y_test.txt", header = FALSE)  ##imports test class label data
testX <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)  ##imports test result data
TestSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)  ##imports the test subject data

### check if all the data sets needed are prepared
ls()

### rename the columns of data sets
colnames(testX) <- features[,2]
colnames(testY) <- "LabelName"
colnames(TestSubject) <- "Subject"
colnames(trainingX) <- features[,2]
colnames(trainingY) <- "LabelName"
colnames(TrainingSubject) <- "Subject"
colnames(activities) <- c("ActivityLabel", "ActivityName")

### merge the data sets into the "DB" data set
TestData <- cbind(TestSubject, testY, testX)
TrainingData <- cbind(TrainingSubject, trainingY, trainingX)
DB <- rbind(TrainingData, TestData)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
DB[,grepl("(mean|std)\\(\\)",colnames(DB))]

## 3. Uses descriptive activity names to name the activities in the data set
### mark every value in "Activity Name" with its real name
DB <- merge(DB, activities, by.x ="LabelName", by.y = "ActivityLabel")
### move the column "ActivityName" next to Column"ActivitLabel" for better framework
ncol(DB)
DBtruncation <- DB[,2:563]
colnames(DB) <- c("ActivityLabel", "ActivityName", colnames(DBtruncation))


## 4. Appropriately labels the data set with descriptive variable names. 
### Sorting the column names
ColNames <- colnames(DB)
for (i in 1:length(ColNames)) {
	ColNames[i] <- gsub("\\(\\)","", ColNames[i])
	ColNames[i] <- gsub("mean","Mean", ColNames[i])
	ColNames[i] <- gsub("std","StandardDeviation", ColNames[i])
	ColNames[i] <- gsub("mad","MediaAbsoluteDeviation", ColNames[i])
	ColNames[i] <- gsub("max","Max", ColNames[i])
	ColNames[i] <- gsub("min","Min", ColNames[i])
	ColNames[i] <- gsub("sma?","SignalMagnitudeArea", ColNames[i])
	ColNames[i] <- gsub("iqr","InterquartileRange", ColNames[i])
	ColNames[i] <- gsub("^t","Time\\-",ColNames[i])
	ColNames[i] <- gsub("^f","Frequency\\-",ColNames[i])
	ColNames[i] <- gsub("[Aa]cc","Accelerometer",ColNames[i])
	ColNames[i] <- gsub("[Gg]yro","Gyroscope", ColNames[i])
	ColNames[i] <- gsub("[Mm]ag","Magnitude", ColNames[i])
}
### Assigning the new column names to the "DB" data set
colnames(DB) <- ColNames

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
### create a data set without Activity Names
newDB <- DB[, -2]
### extract the feature variables
FeatureDataName <- colnames(DB[, 4:564])

### calculate the average of each variable for each activity and each subject
aggregate <- aggregate(newDB[, FeatureDataName], by = list(Activity = newDB$ActivityLabel, Subject = newDB$Subject), mean)  
aggregate <- merge(aggregate, activities, by.x = "Activity", by.y = "ActivityLabel")

### export the tidy data set
write.table(aggregate, "./TidyData.txt", row.name=FALSE)
