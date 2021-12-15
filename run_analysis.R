library(dplyr)
library(fame)

# create tidy data of the training data and label as training (train$train/test = 1)
setwd('D:/practice/r/data_science/final_proj/UCI HAR Dataset/')
train_sub <- read.table("train/subject_train.txt")
colnames(train_sub) <- "Subjects"

train_labels <- read.table("train/y_train.txt")
colnames(train_labels) <- "Labels"

train_x <- read.table("train/X_train.txt")
features <- read.table("features.txt")
colnames(train_x) <- features$V2

train_test <- data.frame(rep(1,nrow(train_x)))
colnames(train_test) <- "train/test"

train <- cbind(train_sub,train_test, train_labels,train_x)

# create tidy data of the test data and label as test (train$train/test = 2)
test_sub <- read.table("test/subject_test.txt")
colnames(test_sub) <- "Subjects"

test_labels <- read.table("test/y_test.txt")
colnames(test_labels) <- "Labels"

test_x <- read.table("test/X_test.txt")
colnames(test_x) <- features$V2

train_test <- data.frame(rep(2,nrow(test_x)))
colnames(train_test) <- "train/test"

test <- cbind(test_sub,train_test, test_labels,test_x)

#combine training and data data frames
data <- rbind(train,test)

#excluding variables that is not the mean or standard deviations
new_data <- cbind(data[,1:3],select(data, matches("mean()|std()")))

# change activity labels
new_lab <- new_data

#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING
new_lab$Labels[new_data$Labels == 1] <- "walking"
new_lab$Labels[new_data$Labels == 2] <- "walking_upstairs"
new_lab$Labels[new_data$Labels == 3] <- "walking_downstairs"
new_lab$Labels[new_data$Labels == 4] <- "sitting"
new_lab$Labels[new_data$Labels == 5] <- "standing"
new_lab$Labels[new_data$Labels == 6] <- "laying"

# calculate average for every subject and label

avg_data <- data.frame(matrix(data = 0,ncol = ncol(new_lab), nrow = length(unique(new_lab$Subjects))*length(unique(new_lab$Labels))))
colnames(avg_data) <- colnames(new_lab)

a <- 0

for(ii in unique(new_lab$Subjects)){
  for(ee in unique(new_lab$Labels)){
    a <- a + 1
    new_mat <- new_lab[new_lab$Subjects==ii & new_lab$Labels==ee,4:ncol(new_lab)]
    avg_data$Subjects[a] <- ii
    avg_data$Labels[a] <- ee
    avg_data[a,4:ncol(new_lab)] <- colMeans(new_mat)
  }
}

avg_data <- avg_data[,-2]
write.table(avg_data,file = "samsung_tidy_data.txt", row.names = FALSE, sep=",")
