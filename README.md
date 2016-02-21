# Getting_Cleanning_Data_Project
Getting and Cleaning Data Course Project
This is a code book that describes the transformations and tidy regarding files generated in experience of the Sansung.

# Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

# The data source
	Original data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
	Original description of the dataset:http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# The attributes - Information
The dataset is then divided in two parts and they can be used separately. 

1. Inertial sensor data 
- Raw triaxial signals from the accelerometer and gyroscope of all the trials with with participants. 
- The labels of all the performed activities. 

2. Records of activity windows. Each one composed of: 
- A 561-feature vector with time and frequency domain variables. 
- Its associated activity label. 
- An identifier of the subject who carried out the experiment

# The attributes - Objects
-  features_info.txt: Shows information about the variables used on the feature vector. 

-  features.txt: List of all features. 

-  activity_labels.txt: Links the activity ID with their activity name. 

-  x_train.txt: Training set. 

-  y_train.txt: Training labels. 

-  x_test.txt: Test set. 

-  y_test.txt: Test labels. 

-  subject_train.txt: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

-  subject_test.txt: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
Below, follow details of the files and data defined in R.
subject_train.txt = tableSubTrain
subject_test.txt = tableSubTest
Y_train.txt = tableActTrain
Y_test.txt = tableActTest
X_train.txt = tableTrain
X_test.txt = tableTest
features.txt = tableFeaturesNames
activity_labels.txt = tableActLabels

# Transformation details
1.	Select all files and Merges the training and the test sets. After, it will combine results and texts to generate a data set.
2.	Define the mean and standard deviation for each measurement.
3.	Define labels the data set with descriptive activity names.
4.	Generate a new tidy data tidy data set with the average of each variable for each activity and each subject.
