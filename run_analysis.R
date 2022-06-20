library(tidyverse)
library(dplyr)
library(readr)
library(data.table)
library(tibble)



# download compressed data
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "data.zip")

# extract data
unzip(zipfile = "data.zip")


# Tasks 1,3,4 (Tasks 2 and 5 to be done later in the code)
# 1. Merge the training and the test sets to create one data set
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive variable names. 

## create a list of the 6 activity names and update the column names
activity_labels <- read_table("activity_labels.txt", col_names = FALSE)
colnames(activity_labels) <- c("label", "activity")

## import the training and test labels and update the column names
y_train <- read_table("train/y_train.txt", col_names = FALSE)
colnames(y_train) <- c("label")

y_test <- read_table("test/y_test.txt", col_names = FALSE)
colnames(y_test) <- c("label")


## apply the activity names to y_train and y_test
y_train_label <- left_join(y_train, activity_labels, by = "label")
y_train_label <- subset(y_train_label, select = "activity")

y_test_label <- left_join(y_test, activity_labels, by = "label")
y_test_label <- subset(y_test_label, select = "activity")


## import the 561 features and retain only the descriptions
features <- read_table("features.txt", col_names = FALSE)
features <- subset(features, select = X2)
col_labels_t <- transpose(features)


## import and merge the training data set
### 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
### 'train/X_train.txt': Training set.
subject_train <- read_table("train/subject_train.txt", col_names = FALSE)
colnames(subject_train) <- c("subject")
X_train <- read_table("train/X_train.txt", col_names = FALSE)
colnames(X_train) <- col_labels_t
train_set <- cbind(subject_train, y_train_label, X_train)


## import and merge the test data set
subject_test <- read_table("test/subject_test.txt", col_names = FALSE)
colnames(subject_test) <- c("subject")
X_test <- read_table("test/X_test.txt", col_names = FALSE)
colnames(X_test) <- col_labels_t
test_set <- cbind(subject_test, y_test_label, X_test)


## merge the test and training data sets
full_data <- rbind(train_set, test_set)


## remove data frames no longer needed
rm(X_test, X_train, y_test, y_train, subject_test, subject_train, y_test_label, y_train_label, features, col_labels_t, activity_labels)


# Task
# 2. Extract only the measurements on the mean and standard deviation for each measurement
mean_std_subset <- select(full_data, contains("subject") | contains("activity") | contains("mean") | contains("std"))


# Task
# 5. From the data set in step 4, create a second, independent tidy data set with 
# the average of each variable for each activity and each subject.

## make all the column names unique
names(full_data) <- make.names(names(full_data), unique = TRUE)

## group by "subject" and "activity"
data_5 <- full_data %>%
        group_by(subject, activity) %>%
        summarize(across(everything(), list(mean)))

write.table(data_5, "data_5.txt", row.names = FALSE)
