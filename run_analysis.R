# 1. Merges the training and the test sets to create one data set.

#Read activity tables and features
actLabels <- read.table('./UCI HAR Dataset/activity_labels.txt', 
                        col.names = c('activityLabels', 'activityName'), quote = "")
features <- read.table('./UCI HAR Dataset/features.txt', 
                       col.names = c('featureLabels', 'featureName'), quote = "")

#Reading and combining testing data
subTest <- read.table('./UCI HAR Dataset/test/subject_test.txt', col.names = c('subjectId'))
XTest <- read.table('./UCI HAR Dataset/test/X_test.txt')
yTest <- read.table('./UCI HAR Dataset/test/y_test.txt')

colnames(XTest) <- features$featureName
colnames(yTest) <- c('activityLabels')
testdata <- cbind(subTest, XTest, yTest)

#Reading and combining training data
subTrain <- read.table('./UCI HAR Dataset/train/subject_train.txt', col.names = c('subjectId'))
XTrain <- read.table('./UCI HAR Dataset/train/X_train.txt')
yTrain <- read.table('./UCI HAR Dataset/train/y_train.txt')

colnames(XTrain) <- features$featureName
colnames(yTrain) <- c('activityLabels')
traindata <- cbind(subTrain, XTrain, yTrain)

#Combing training and testing data to get full dataset
fulldata <- rbind(traindata, testdata)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

meanSD <- fulldata[, c(1, grep(pattern = 'mean\\(\\)|std\\(\\)', x = names(fulldata)), 563)]
#select only variables with mean and std - excludes meanFreq() and angle()
#(refer to features_info.txt for more information)
#also include the subject ID (col = 1) and activity code (col = 563)


# 3. Use descriptive activity names to name the activities in the data set.

meanSD$subjectId <- as.factor(meanSD$subjectId)
meanSD$activity <- factor(meanSD$activityLabels,
                          levels = actLabels$activityLabels,
                          labels = actLabels$activityName)

meanSD <- meanSD[, -68]  #remove the activity labels column to tidy up the data
names(meanSD)


# 4. Appropriately labels the data set with descriptive variable names.

colnames(meanSD) <- gsub(pattern = '\\(\\)', replacement = "", x = names(meanSD)) #remove the () for the mean and std in the measurements
meanSD <- meanSD[, c(1, 68, 2:67)]
write.table(meanSD, file = 'tidyData.txt', row.names = F, quote = F, sep = "\t")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)
meanSDByActID <- group_by(meanSD, subjectId, activity) %>% summarise_all(funs(mean))
write.table(meanSDByActID, file = 'tidyDataMean.txt', row.names = F, quote = F, sep = "\t")