==================================================================
Tidying Human Activity Recognition using Smartphones Dataset
Version 1.0
==================================================================
Dr.D.Kavitha
==================================================================
Human Activity Recognition Using Smartphones Dataset consisted of train data set with 7352 objects, test data set with 2947 objects and with 561 features each.
The sensors accelerometer and gyroscope are used to estimate the following features
tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 
mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.


First the X_train data set has been extracted along with subject_train and y_train datasets.
These three train data sets are combined to form a train data set with 7352 objects consisting of 561 features, subject, activity variable.
Similarly, X_test dataset, subject_test, y_test datasets have been extracted consisting of 2947 objects with 561 features, subject and activity variable.

Then train and test data sets have been merged to form a total of 10,299 objects with 563 variables.

Next, the titles of all 561 features are extracted from features.txt file. 
From this 561 features, duplicate features are eliminated and 477 unique features have been left.
From this 477 features, only those features consisting of mean and standard deviation are retrieved.
The activity variable column is replaced with descriptive activity labels.
The resulting data set is grouped by subject and activity.
Finally the tidy data set is created with the average of each  feature grouped by subject and activity.



Following files are present in the repository
=========================================

- 'README.md�

- �codeBook.md

- runAnalysis.r

