# Getting-and-Cleaning-Data

### My work on the course project for Coursera course "Getting and Cleaning Data"

Although I usually do not stuff my files like this, I decided to keep all the functionality in the run_analysis.R skript.  I hope this makes it easier to you, the reader, to review it if all information is kept in one place.  But I did wrap the code into a function, so it is possible to specify three arguments:

* base_path: This is the path to the base directory of the data set. By default it is "UCI HAR Dataset", so if the UCI HAR Dataset directory is in your home directory, the function should work by default
* txt.output: If set to TRUE, the function will write the tidy data set to the file "tidy_set.txt". The default value is FALSE
* return.both: If set to TRUE, the tidy data set will be returned as output of the function, and the big data set with all measurements for the mean() and std() variables will be written in tidy form to the global environment using scoping. Default is FALSE, which will only return the tidy data set as output of the function

Usage: tidy_set <- run_analysis(base_path="UCI HAR Dataset", txt.output=FALSE, return.both=FALSE)

The steps of the assignment are performed in the order specified in the assignment description on coursera.org. As the code is extensively commented, I do not describe the function here.

Some points to note:

* The script requires the **plyr and tidyr libraries**
* I combinded the test and training data sets in the order: test, train
* I sorted the data set by subject id and descriptive name of activity performed before creating the tidy set