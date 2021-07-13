######################################################################
# Downloading and prepare all possible use libraries 
install.packages("data.table", dependencies=TRUE)
install.packages("plyr")
install.packages("reshape2")
library(data.table)
library(plyr)
library(reshape2)

######################################################################
# Creating data file in ".Rproj" and downloading the data
dir.create("./data")
onlinefile <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/data.zip",method="curl")
unzip(zipfile="./data/Dataset.zip",exdir="./data")

######################################################################
# Merging "train" and "test" of each database (X, Y and subject)   
# X                                                                       
Xtrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
Xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
X <- rbind(Xtrain, Xtest)
# Y
Ytrain <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
Ytest <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")
Y <- rbind(Ytrain, Ytest)
# Subject
SUBJECTtrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
SUBJECTtest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
SUBJECT <- rbind(SUBJECTtrain, SUBJECTtest)

######################################################################
# Creating a table only with means and standard deviations
features <- read.table(paste(sep = "", "./data/UCI HAR Dataset/features.txt"))
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X2 <- X[, indices_of_good_features]
names(X2) <- features[indices_of_good_features, 2]
names(X2) <- gsub("\\(|\\)", "", names(X2))
names(X2) <- tolower(names(X2))

######################################################################
# Adding the columns subject and activity
activities <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"
names(SUBJECT) <- "subject"

######################################################################
# Gathering subject, activity and all data to create one table
tidydata <- cbind(SUBJECT, Y, X)
write.table(tidydata, "tidydata.txt")


######################################################################
# Average of each variable for activities and subjects
uniqueSubjects = unique(SUBJECT)[,1]
SubjectsCols = length(unique(SUBJECT)[,1])
ActivitiesCols = length(activities[,1])
numCols = dim(tidydata)[2]
tidydate2 = tidydata[1:(SubjectsCols*ActivitiesCols), ]
row = 1
for (s in 1:SubjectsCols) {
  for (a in 1:ActivitiesCols) {
    tidydate2[row, 1] = uniqueSubjects[s]
    tidydate2[row, 2] = activities[a, 2]
    w <- tidydata[tidydata$subject==s & tidydata$activity==activities[a, 2], ]
    tidydate2[row, 3:numCols] <- colMeans(w[, 3:numCols])
    row = row+1 } }

write.table(tidydate2, "tidydata_final.txt")
tidydate2
