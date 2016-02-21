#run_analysis.R

###Load packages
library(dplyr)
library(data.table)
library(tidyr)

# Download data defined and after analise and tide data.
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#setwd("C:/Users/luiz.bissoli/Documents/Coursera/2-Get/Project/data")
path <- paste(getwd(), "Dataset.zip", sep = "/")
download.file(fileUrl, path)

#Unzip File to work
unzip(zipfile=path,exdir="./data")

setwd("./data")
path <- paste(getwd(), "UCI HAR Dataset", sep = "/")

# Read Files
tableSubTrain <- tbl_df(read.table(file.path(path, "train", "subject_train.txt")))
tableSubTest  <- tbl_df(read.table(file.path(path, "test" , "subject_test.txt" )))
tableActTrain <- tbl_df(read.table(file.path(path, "train", "Y_train.txt")))
tableActTest  <- tbl_df(read.table(file.path(path, "test" , "Y_test.txt" )))
tableTrain    <- tbl_df(read.table(file.path(path, "train", "X_train.txt" )))
tableTest     <- tbl_df(read.table(file.path(path, "test" , "X_test.txt" )))


# Concatenate the Tables
tableSubject  <- rbind(tableSubTrain, tableSubTest)
tableActivity <- rbind(tableActTrain, tableActTest)
tableFeature  <- rbind(tableTrain,    tableTest)

# Define name to the columns
setnames(tableSubject,  "V1", "subject")
setnames(tableActivity, "V1", "NumAct")

# name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
tableFeaturesNames <- tbl_df(read.table(file.path(path, "features.txt")))
colnames(tableFeature) <- tableFeaturesNames$V2

# activity labels - read and merge
tableActLabels <- tbl_df(read.table(file.path(path, "activity_labels.txt")))
setnames(tableActLabels, names(tableActLabels), c("NumAct","NameAct"))
tableSubjAct<- cbind(tableSubject, tableActivity)
tableFeature <- cbind(tableSubjAct, tableFeature)

# Reading "features.txt" and extracting only the mean and standard deviation
tableFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",tableFeaturesNames$V2,value=TRUE)

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

tableFeaturesMeanStd <- union(c("subject","NumAct"), tableFeaturesMeanStd)
tableFeature <- subset(tableFeature,select=tableFeaturesMeanStd) 

##name of activity
tableFeature <- merge(tableActLabels, tableFeature , by="NumAct", all.x=TRUE)
tableFeature$NameAct <- as.character(tableFeature$NameAct)

## DataPrint with variable means sorted by subject and Activity
tablePrint <- aggregate(. ~ subject - NameAct, data = tableFeature, mean) 
tableFeature <- tbl_df(arrange(tablePrint,subject,NameAct))

# Define complet name
names(tableFeature)<-gsub("std()", "SD", names(tableFeature))
names(tableFeature)<-gsub("mean()", "MEAN", names(tableFeature))
names(tableFeature)<-gsub("^t", "time", names(tableFeature))
names(tableFeature)<-gsub("^f", "frequency", names(tableFeature))
names(tableFeature)<-gsub("Acc", "Accelerometer", names(tableFeature))
names(tableFeature)<-gsub("Gyro", "Gyroscope", names(tableFeature))
names(tableFeature)<-gsub("Mag", "Magnitude", names(tableFeature))
names(tableFeature)<-gsub("BodyBody", "Body", names(tableFeature))

# Generate the file
write.table(tableFeature, "TidyData.txt", row.name=FALSE)
