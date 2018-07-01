#load the dplyr package

library(dplyr)

#download the file/dataset if not already on a local drive
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "UCI HAR Dataset.zip"

if (!file.exists(fileName)) {
  download.file(fileURL, fileName, mode = "wb")
}

#unzip data folder if not already done
data_name <- "UCI HAR Dataset"
if (!file.exists(data_name)) {
  unzip(fileName)
}

#read training data
subject_train <- read.table(file.path(data_name, "train", "subject_train.txt"))
x_train <- read.table(file.path(data_name, "train", "X_train.txt"))
y_train <- read.table(file.path(data_name, "train", "y_train.txt"))

# read test data
subject_test <- read.table(file.path(data_name, "test", "subject_test.txt"))
x_test <- read.table(file.path(data_name, "test", "X_test.txt"))
y_test <- read.table(file.path(data_name, "test", "y_test.txt"))

#read features
features <- read.table(file.path(data_name, "features.txt"), as.is = TRUE)

# read activity labels
activity_labels <- read.table(file.path(data_name, "activity_labels.txt"))
colnames(activity_labels) <- c("activityId", "activityLabel")

#merge indivual tables into one table
subjectData <- rbind(
  cbind(subject_train, x_train, y_train),
  cbind(subject_test, x_test, y_test)
)

# assign column names
colnames(subjectData) <- c("subject", features[, 2], "activity")

# Extract only the measurements on the mean and standard deviation by reading column names
necessaryColumns <- grepl("subject|activity|mean|std", colnames(subjectData))

#remove unnecessary columns from master data set
subjectData <- subjectData[, necessaryColumns]

# replace activity values with provided factor levels
subjectData$activity <- factor(subjectData$activity, 
      levels = activity_labels[, 1], labels = activity_labels[, 2])

# get column names
subjectDataColumns <- colnames(subjectData)

# remove special characters
subjectDataColumns <- gsub("[\\(\\)-]", "", subjectDataColumns)

# Appropriately label the data set with descriptive variable names
subjectDataColumns <- gsub("^f", "Frequency Domain", subjectDataColumns)
subjectDataColumns <- gsub("^t", "Time Domain", subjectDataColumns)
subjectDataColumns <- gsub("Acc", "Accelerometer", subjectDataColumns)
subjectDataColumns <- gsub("Gyro", "Gyroscope", subjectDataColumns)
subjectDataColumns <- gsub("Mag", "Magnitude", subjectDataColumns)
subjectDataColumns <- gsub("Freq", "Frequency", subjectDataColumns)
subjectDataColumns <- gsub("mean", "Mean", subjectDataColumns)
subjectDataColumns <- gsub("std", "Standard Deviation", subjectDataColumns)
subjectDataColumns <- gsub("BodyBody", "Body", subjectDataColumns)

#set descriptive variable names as column titles
colnames(subjectData) <- subjectDataColumns

# use mean to summarize subject/activity data groups
subjectDataMeans <- subjectData %>% 
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

# create second independent tidy data set
write.table(subjectDataMeans, "tidy_data_set.txt", row.names = FALSE, 
            quote = FALSE)