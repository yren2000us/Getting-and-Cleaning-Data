setwd('/Users/heather/Documents/UCI HAR Dataset/');

##########################################################################################################
##													##
## Getting and Cleaning Data Course Project								##
## Yuan Ren												##
## July 2016												##
##													##
## The goal is to prepare tidy data that can be used for later analysis:				##
## The program will perform the following 								##
## 1. Merges the training and the test sets to create one data set.					##
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.		##
## 3. Uses descriptive activity names to name the activities in the data set				##
## 4. Appropriately labels the data set with descriptive variable names.				##
## 5. From the data set in step 4, creates a second, independent tidy data set with the average 	##
##    of each variable for each activity and each subject.						##
##########################################################################################################
library(plyr)
library(reshape2)
library(dplyr)

# Step 1. Merge the training and the test data.

#set working directory to where the UCI HAR Dataset was unzipped
setwd("C:/Users/21722/Desktop/Coursera/Getting and Cleaning Data/UCI HAR Dataset");

# Input data files

train_x <- read.table("UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

test_x <- read.table("UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

train <- cbind(train_subject, train_y, train_x)
test <- cbind(test_subject, test_y, test_x)
Alldata <- rbind(train, test)

## Setp 2. Extract only the measurements on the mean and standard deviation for each measurement. 

## load feature name into R
featureName <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[,2]

## extract mean and standard deviation of each measurements
featureIndex <- grep(("mean\\(\\)|std\\(\\)"), featureName)
finalData <- Alldata[, c(1, 2, featureIndex+2)]
colnames(finalData) <- c("subject", "activity", featureName[featureIndex])

## 3. Uses descriptive activity names to name the activities in the data set	

activityName <- read.table("UCI HAR Dataset/activity_labels.txt")
finalData$activity <- factor(finalData$activity, levels = activityName[,1], labels = activityName[,2])

## 4. Appropriately labels the data set with descriptive variable names.

names(finalData) <- gsub("\\()", "", names(finalData))
names(finalData) <- gsub("^t", "time", names(finalData))
names(finalData) <- gsub("^f", "frequence", names(finalData))
names(finalData) <- gsub("-mean", "Mean", names(finalData))
names(finalData) <- gsub("-std", "Std", names(finalData))

## 5. From the data set in step 4, creates a second, independent tidy data set with the 
##   average of each variable for each activity and each subject.

groupData <- finalData %>%
        group_by(subject, activity) %>%
        summarise_each(funs(mean))

write.table(groupData, "./tidyData.txt", row.names = FALSE)