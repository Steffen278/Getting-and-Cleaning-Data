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
activityLabelsFile <- "UCI HAR Dataset/activity_labels.txt"

## Feature List is used to calculate the number of rows for the two data sets
featureListFile <- "UCI HAR Dataset/features.txt"

## Reading in the raw data

test_X_raw <- scan(testFile, what=numeric())
train_X_raw <- scan(trainFile, what=numeric())
subject_test_number <- scan(testSubjectFile)
subject_train_number <- scan(trainSubjectFile)
activity_test_number <- scan(testActivityFile, what=character())
activity_train_number <- scan(trainActivityFile, what=character())
activityLabels <- scan(activityLabelsFile, what=character())

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

subject_number <- c(subject_test_number, subject_train_number)
activity_number <- c(activity_test_number, activity_train_number)

## Remove the unneeded data to conserve space and to declutter

rm(test_X_raw); rm(train_X_raw); rm(featureList_raw)
rm(test_X_matrix); rm(train_X_matrix)
rm(test_X_df); rm(train_X_df)


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


###############################################################
# Step 3: Replace the activity numbers with descriptive names #
###############################################################

## Creating a vector of descriptive activity labels
## This is facilitated by an anonymous function, which compares the activity number from the data set with the activity number in the
## activity labels file and returns its index. Since the structure of the activity labels file is "activity #" "activity description",
## the index has to be incremented by one to return the activity label

activity_labels <- sapply(activity_number, function(x) {activityLabels[grep(x, activityLabels)+1]})

## Bind the activity labels vector to the combinded data  frame

merged_set <- cbind(activity_labels, merged_set)

## Bind the subject information to the combinded data frame

merged_set <- cbind(subject_number, merged_set)


#############################################
# Step 4: Adding descriptive variable names #
#############################################



## Final cleanup to declutter the work space

rm(testFile); rm(trainFile); rm(testSubjectFile); rm(trainSubjectFile); rm(featureListFile); rm(testActivityFile); rm(trainActivityFile)
rm(activityLabelsFile)
rm(featureList_names); rm(index); rm(subject_number); rm(activity_number); rm(activity_test_number); rm(activity_train_number)
rm(subject_test_number); rm(subject_train_number)
rm(activity_labels); rm(activityLabels)