### Codebook for the project work on the Coursera Course "Getting and Cleaning Data"

The input for the run_analysis script is the UCI "Human Activity Recognition Using Smartphones Dataset" Version 1 provided by Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio and Luca Oneto.

[The original data can be found here](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

**From the original description of the data set:**

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:

* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
* Triaxial Angular velocity from the gyroscope. 
* A 561-feature vector with time and frequency domain variables. 
* Its activity label. 
* An identifier of the subject who carried out the experiment.

====================================================================================================================

I performed all steps of the analysis in the order specified by the assignment:

**Step 1:** I read the test and training data in as numeric values and combine them using rbind in the order: test data, training data

**Step 2:** I extract from the original data all variables containing the terms "mean" or "std" for standard deviation. This was specified in the assignment, and I do not feel qualified to decide which means and SDs to include or exclude

**Step 3:** Using sapply() I exchange all activity ids from the combined data set with their lower case text labels as given by the activity_labels.txt. Using cbind I bind the labels and subject ids to the data set, so that it now has the structure:

subject_id, activity_labels, mean and SD variables

**Step 4:** I convert the variable names:

* Convert all letters to lower case
* Replace the - with _
* Get rid of the ()
* Change the prefixes: t to time, f to frequency
* Change gyro to Gyroscope, acc to Acceleration
* Change mag to Magnitude
* Change std to SD
* Meanfreq to MeanFrequency
* For x,y,z at the end of the variable name write "x,y,z-Axis" respectively
* Use CamlCase on all variable names

**Interlude:** I tidy up the whole data set using gather()_, putting all variables in the column "measurement_type" and all measurements in the column "measurement_value".

Afterwards I rearrange the data by subject id and activity (descriptive label).

**Step 5:** I create a tidy data set which includes the mean value for the measurements for each combination of subject id, activity and variable.

This leaves me with the columns "subject_id", "activity_labels", "measurement_type" and "mean_measurement".
