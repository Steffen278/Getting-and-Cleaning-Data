## Raw prototype for reading in the data

######################################################
# Step 1: Reading the data and merging the data sets #
######################################################

## File names are one directory down, then two up as specified below

## BEFORE SUBMITTING: SKRIPT SHOULD WORK FROM HOME DIR - SO NO PATH!!!!!!

#testFile <- "../UCI HAR Dataset/test/X_test.txt"
#trainFile <- "../UCI HAR Dataset/train/X_train.txt"

testFile <- "UCI HAR Dataset/test/X_test.txt"
trainFile <- "UCI HAR Dataset/train/X_train.txt"
testSubjectFile <- "UCI HAR Dataset/test/subject_test.txt"
trainSubjectFile <- "UCI HAR Dataset/train/subject_train.txt"
testActivityFile <- "UCI HAR Dataset/test/y_test.txt"
trainActivityFile <- "UCI HAR Dataset/train/y_train.txt"

## Feature List is used to calculate the number of rows for the two data sets
featureListFile <- "UCI HAR Dataset/features.txt"

## Reading in the raw data

test_X_raw <- scan(testFile, what=character())
train_X_raw <- scan(trainFile, what=character())
subject_test <- scan(testSubjectFile)
subject_train <- scan(trainSubjectFile)
activity_test <- scan(testActivityFile)
activity_train <- scan(trainActivityFile)

featureList_raw <- scan(featureListFile, what=character())

## Since the features.txt if of form: # name # name ... we need to only use the names (every 2nd element)
## This also gives the vector the same length as the number of columns in the data sets (since it contains the column names)

featureList_names <- featureList_raw[seq(2, length(featureList_raw), by=2)]

## Giving the data shape: Creating matrices, where the number of rows is derived by the length of the datasets divided by the number of columns
## as given by the length of the featureList_raw vector (561 as per README file)

test_X_matrix <- matrix(test_X_raw, length(test_X_raw)/length(featureList_names), length(featureList_names))
train_X_matrix <- matrix(train_X_raw, length(train_X_raw)/length(featureList_names), length(featureList_names))

## Convert the matrices to data frames

test_X_df <- data.frame(test_X_matrix)
train_X_df <- data.frame(train_X_matrix)

## Name the columns of the data frames

names(test_X_df) <- featureList_names
names(train_X_df) <- featureList_names

## Merge the to data frames into one

merged_set <- rbind(test_X_df, train_X_df)

## Create one vector of subject information and activity information

subject <- c(subject_test, subject_train)
activity <- c(activity_test, activity_train)

## Remove the unneeded data to conserve space and to declutter

rm(test_X_raw); rm(train_X_raw); rm(featureList_raw)
rm(test_X_matrix); rm(train_X_matrix)
rm(test_X_df); rm(train_X_df)
rm(subject_test); rm(subject_train)

##################################################################
# Step 2: Extracting the mean and standard deviation measurments #
##################################################################

## Finding the columns in the combined data frame which include data about the mean

index <- grep("mean", names(merged_set))

## Add the columns from the combinded data frame which include data about the standard deviation

index <- append(index, grep("std", names(merged_set)))

## Index now contains the columns numbers of the mean and standard deviation measurements, in that order
## Removing all non relevant columns from the combined data frame

merged_set <- merged_set[,index]

## Bind the subject information and activity information to the data frame

merged_set <- cbind(activity, merged_set)
merged_set <- cbind(subject, merged_set)

## Final cleanup to declutter the work space

rm(testFile); rm(trainFile); rm(testSubjectFile); rm(trainSubjectFile); rm(featureListFile); rm(testActivityFile); rm(trainActivityFile)
rm(featureList_names); rm(index); rm(subject); rm(activity); rm(activity_test); rm(activity_train)