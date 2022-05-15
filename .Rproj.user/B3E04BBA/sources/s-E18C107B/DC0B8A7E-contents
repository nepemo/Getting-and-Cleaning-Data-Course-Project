library("dplyr")

features <- read.table("./UCI HAR Dataset/features.txt")
features$V2 <- gsub("-","_",features$V2)
features$V2 <- gsub("\\(\\)","_",features$V2)
#read test sets
test_X <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features$V2)
test_activity <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "activity")
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
#read train sets
train_X <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features$V2)
train_activity <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "activity")
train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

#1. merge datasets into one sets
test_sets <- cbind(test_X, test_activity, test_subject)
train_sets <- cbind(train_X, train_activity, train_subject)
raw_datasets <- rbind(test_sets, train_sets)
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
col <- grepl("\\_mean\\_|\\_std\\_|activity|subject", colnames(raw_datasets))
datasets <- raw_datasets[,col]
#3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity_func <- function(x){
    x <- activity[x,2]
}
datasets$activity <- sapply(datasets$activity, activity_func)
#4. Appropriately labels the data set with descriptive variable names. 
colnames(datasets) <- gsub("mean\\_", "mean", colnames(datasets))
colnames(datasets) <- gsub("std\\_", "std", colnames(datasets))
#5. a second data set with the average of each variable for each activity and each subject.
grouped_datasets <- aggregate(datasets, by=list(datasets$activity, datasets$subject), mean)
grouped_datasets <- select(grouped_datasets, -c(activity,subject))
grouped_datasets <- rename(grouped_datasets, activity = Group.1, subject = Group.2)

write.table(grouped_datasets, file="./Getting and Cleaning Data Course Project.txt",
            row.names=FALSE)
