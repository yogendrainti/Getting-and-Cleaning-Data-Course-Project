## Load "dyplr" package
library(dyplr)

## Set data set directory
setwd("~/Desktop/Coursera Data Science/UCI HAR Dataset")

## read train and test data set
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/Y_train.txt")
subject_train <- read.table("./train/subject_train.txt")

x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/Y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

## Merge train and test data sets
x_total <- rbind(x_train, x_test)
y_total <- rbind(y_train, y_test)
subject_total <- rbind(subject_train, subject_test)

## read features description and activity labels
features <- read.table("./features.txt")
activity_labels <- read.table("./activity_labels.txt")

## Extract only the measurements on the mean and standard deviation for 
## each measurement
select_features <- variable.names[grep(".*mean\\(\\) | std\\(\\)", features[,2],
                                       ignore.case = FALSE), ]
x_total <- x_total[, select_features[,1]]

## assigning column names
colnames(x_total) <- select_features[,2]
colnames(y_total) <- "activity"
colnames(subject_total) <- "subject"

## merge final data set
total <- cbind(subject_total, y_total, x_total)

## turn activities and subjects into factors
total$activity <- factor(total$activity, levels = activity_labels[,1], 
                         labels = activity_labels[,2])
total$subject <- as.factor(total$subject)

## create a summary of independent tidy data set from final data set with the
## average of each variable for each activity and subject
total_mean <- total %>%
              group_by(activity, subject) %>%
              summarize_all(mean)

## export and save the summary data set as .txt file
write.table(total_mean, file = "./tidydata.txt", row.names = FALSE, 
            col.names = TRUE)

