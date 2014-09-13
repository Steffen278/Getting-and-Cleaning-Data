## Raw prototype for reading in the data

## Loading libraries which we are going to use

library(plyr)
library(tidyr)

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
subject_test_id <- scan(testSubjectFile)
subject_train_id <- scan(trainSubjectFile)
activity_test_id <- scan(testActivityFile, what=character())
activity_train_id <- scan(trainActivityFile, what=character())
activityLabels_raw <- scan(activityLabelsFile, what=character())

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

subject_id <- c(subject_test_id, subject_train_id)
activity_id <- c(activity_test_id, activity_train_id)

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


###########################################################
# Step 3: Replace the activity ids with descriptive names #
###########################################################

## Creating a vector of descriptive activity labels
## This is facilitated by an anonymous function, which compares the activity id from the data set with the activity id in the
## activity labels file and returns its index. Since the structure of the activity labels file is "activity #" "activity description",
## the index has to be incremented by one to return the activity label
## Also, in the same step, in the sense of continuity, we convert the activity labes to lower case

activity_labels <- sapply(activity_id, function(x) {tolower(activityLabels_raw[grep(x, activityLabels_raw)+1])})

## Bind the activity labels vector to the combinded data  frame

merged_set <- cbind(activity_labels, merged_set)

## Bind the subject information to the combinded data frame

merged_set <- cbind(subject_id, merged_set)


#############################################
# Step 4: Adding descriptive variable names #
#############################################

## Create a temp data set for debugging

debugset <- merged_set

## Convert to all lower cases

names(merged_set) <- tolower(names(merged_set))

## Replace the dash with an underscore

names(merged_set) <- gsub("-", "_", names(merged_set))

## Loose the (). Note the \\ to escape the (), which is a legal regular expression

names(merged_set) <- gsub("\\()", "", names(merged_set))

## Renaming the variables using the names() and gsub() functions with regular expressions

names(merged_set) <- gsub("^f", "frequency_", names(merged_set))
names(merged_set) <- gsub("^t", "time_", names(merged_set))
names(merged_set) <- gsub("body", "Body", names(merged_set))
names(merged_set) <- gsub("jerk", "Jerk", names(merged_set))
names(merged_set) <- gsub("gyro", "Gyroscope", names(merged_set))
names(merged_set) <- gsub("acc", "Acceleration", names(merged_set))
names(merged_set) <- gsub("mag", "Magnitude", names(merged_set))
names(merged_set) <- gsub("mean", "Mean", names(merged_set))
names(merged_set) <- gsub("std", "SD", names(merged_set))


##################################
# Interlude: Tidying up the data #
##################################

## Melt the combined data set using gather_() on all but the subject_id and activity columns
## We use gather_() instead of gather(), as it allows us to use a character to string to define the variables we want to gather,
## thus allowing us to be independent of the actual names and preventing typing errors.
## We know that the first two columns are the subject id and the activty description, so we can gather all other values up to the last
## column

merged_set <- gather_(merged_set, "measurement_type", "measurement_value", c(names(merged_set)[3:length(names(merged_set))]))

## Now that we have the data tidied up we can sort it in a meaningful way, first by subject_id and then by the activity label

merged_set <- arrange(merged_set, subject_id, activity_labels)


##################################################################################################################
# Step 5: Creating a seperate tidy data set with the average of each variable for each activity and each subject #
##################################################################################################################

tidy_set <- ddply(merged_set, .(subject_id, activity_labels, measurement_type), summarise, mean_measurement=mean(measurement_value))

## Final cleanup to declutter the work space

rm(testFile); rm(trainFile); rm(testSubjectFile); rm(trainSubjectFile); rm(featureListFile); rm(testActivityFile); rm(trainActivityFile)
rm(activityLabelsFile)
rm(featureList_names); rm(index); rm(subject_id); rm(activity_id); rm(activity_test_id); rm(activity_train_id)
rm(subject_test_id); rm(subject_train_id)
rm(activity_labels); rm(activityLabels_raw)