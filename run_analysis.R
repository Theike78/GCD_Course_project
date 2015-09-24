project <- function(){
        library(dplyr)

##part 1 Merge the training and the test sets to create one data set.        
##load the general files in to R        
        activity_labels <- read.table("./UCI HAR DATASET/activity_labels.txt",
                                      col.names = c("activity_id","activity"))
        features <- read.table("./UCI HAR DATASET/features.txt",
                               col.names = c("feature_id","feature"))
        colnames <- features[,2]
        colnames <- make.names(colnames,unique = TRUE)
        
##load the test files into R and merge them together
        subject_test <- read.table("./UCI HAR DATASET/test/subject_test.txt",
                                   col.names = c("subject"))
        x_test <- read.table("./UCI HAR DATASET/test/X_test.txt",
                             col.names = colnames)
        y_test <- read.table("./UCI HAR DATASET/test/Y_test.txt",
                             col.names = c("activity_id"))
        full_test_data <- cbind(subject_test,y_test,x_test)

##load the train files info R and merge them together
        subject_train <- read.table("./UCI HAR DATASET/train/subject_train.txt",
                                   col.names = c("subject"))
        x_train <- read.table("./UCI HAR DATASET/train/X_train.txt",
                             col.names = colnames)
        y_train <- read.table("./UCI HAR DATASET/train/Y_train.txt",
                             col.names = c("activity_id"))
        full_train_data <- cbind(subject_train,y_train,x_train)

##put full_test_data and full_train_data together
        full_dataset <- rbind(full_test_data,full_train_data)

##part 2 Extract only the measurements on the mean and standard deviation for each measurement. 
        tidy_dataset <- select(full_dataset, 
                               subject,
                               activity_id,
                               contains("mean",ignore.case = TRUE),
                               contains("std",ignore.case = TRUE))

##part 3 Use descriptive activity names to name the activities in the data set
        tidy_dataset <- inner_join(tidy_dataset,activity_labels,by = NULL,copy = FALSE)
        tidy_dataset <- select(tidy_dataset, -activity_id)

##part 4 Appropriately label the data set with descriptive variable names. 
##This is already done in the previous steps

##part 5 From the data set in step 4, create a second, independent tidy data set 
##with the average of each variable for each activity and each subject.
        tidy_dataset <- read.table("tidy_dataset.txt", header = TRUE)
        tidy_group <- group_by(tidy_dataset, subject, activity)
        final_tidy <- summarise(tidy_group, 
                                tBodyAccmeanX = mean(tBodyAcc.mean...X),
                                tBodyAccmeanY = mean(tBodyAcc.mean...Y),
                                tBodyAccmeanZ = mean(tBodyAcc.mean...Z),
                                tGravityAccmeanX = mean(tGravityAcc.mean...X),
                                tGravityAccmeanY = mean(tGravityAcc.mean...Y),
                                tGravityAccmeanZ = mean(tGravityAcc.mean...Z),
                                tBodyAccJerkmeanX = mean(tBodyAccJerk.mean...X),
                                tBodyAccJerkmeanY = mean(tBodyAccJerk.mean...Y),
                                tBodyAccJerkmeanZ = mean(tBodyAccJerk.mean...Z),
                                tBodyGyromeanX = mean(tBodyGyro.mean...X),
                                tBodyGyromeanY = mean(tBodyGyro.mean...Y),
                                tBodyGyromeanZ = mean(tBodyGyro.mean...Z),
                                tBodyGyroJerkmeanX = mean(tBodyGyroJerk.mean...X),
                                tBodyGyroJerkmeanY = mean(tBodyGyroJerk.mean...Y),
                                tBodyGyroJerkmeanZ = mean(tBodyGyroJerk.mean...Z),
                                tBodyAccMagmean = mean(tBodyAccMag.mean..),
                                tGravityAccMagmean = mean(tGravityAccMag.mean..),
                                tBodyAccJerkMagmean = mean(tBodyAccJerkMag.mean..),
                                tBodyGyroMagmean = mean(tBodyGyroMag.mean..),
                                tBodyGyroJerkMagmean = mean(tBodyGyroJerkMag.mean..),
                                fBodyAccmeanX = mean(fBodyAcc.mean...X),
                                fBodyAccmeanY = mean(fBodyAcc.mean...Y),
                                fBodyAccmeanZ = mean(fBodyAcc.mean...Z),
                                fBodyAccmeanFreqX = mean(fBodyAcc.meanFreq...X),
                                fBodyAccmeanFreqY = mean(fBodyAcc.meanFreq...Y),
                                fBodyAccmeanFreqZ = mean(fBodyAcc.meanFreq...Z),
                                fBodyAccJerkmeanX = mean(fBodyAccJerk.mean...X),
                                fBodyAccJerkmeanY = mean(fBodyAccJerk.mean...Y),
                                fBodyAccJerkmeanZ = mean(fBodyAccJerk.mean...Z),
                                fBodyAccJerkmeanFreqX = mean(fBodyAccJerk.meanFreq...X),
                                fBodyAccJerkmeanFreqY = mean(fBodyAccJerk.meanFreq...Y),
                                fBodyAccJerkmeanFreqZ = mean(fBodyAccJerk.meanFreq...Z),
                                fBodyGyromeanX = mean(fBodyGyro.mean...X),
                                fBodyGyromeanY = mean(fBodyGyro.mean...Y),
                                fBodyGyromeanZ = mean(fBodyGyro.mean...Z),
                                fBodyGyromeanFreqX = mean(fBodyGyro.meanFreq...X),
                                fBodyGyromeanFreqY = mean(fBodyGyro.meanFreq...Y),
                                fBodyGyromeanFreqZ = mean(fBodyGyro.meanFreq...Z),
                                fBodyAccMagmean = mean(fBodyAccMag.mean..),
                                fBodyAccMagmeanFreq = mean(fBodyAccMag.meanFreq..),
                                fBodyBodyAccJerkMagmean = mean(fBodyBodyAccJerkMag.mean..),
                                fBodyBodyAccJerkMagmeanFreq = mean(fBodyBodyAccJerkMag.meanFreq..),
                                fBodyBodyGyroMagmean = mean(fBodyBodyGyroMag.mean..),
                                fBodyBodyGyroMagmeanFreq = mean(fBodyBodyGyroMag.meanFreq..),
                                fBodyBodyGyroJerkMagmean = mean(fBodyBodyGyroJerkMag.mean..),
                                fBodyBodyGyroJerkMagmeanFreq = mean(fBodyBodyGyroJerkMag.meanFreq..),
                                angletBodyAccMeangravity = mean(angle.tBodyAccMean.gravity.),
                                angletBodyAccJerkMeangravityMean = mean(angle.tBodyAccJerkMean..gravityMean.),
                                angletBodyGyroMeangravityMean = mean(angle.tBodyGyroMean.gravityMean.),
                                angletBodyGyroJerkMeangravityMean = mean(angle.tBodyGyroJerkMean.gravityMean.),
                                angleXgravityMean = mean(angle.X.gravityMean.),
                                angleYgravityMean = mean(angle.Y.gravityMean.),
                                angleZgravityMean = mean(angle.Z.gravityMean.),
                                tBodyAccstdX = mean(tBodyAcc.std...X),
                                tBodyAccstdY = mean(tBodyAcc.std...Y),
                                tBodyAccstdZ = mean(tBodyAcc.std...Z),
                                tGravityAccstdX = mean(tGravityAcc.std...X),
                                tGravityAccstdY = mean(tGravityAcc.std...Y),
                                tGravityAccstdZ = mean(tGravityAcc.std...Z),
                                tBodyAccJerkstdX = mean(tBodyAccJerk.std...X),
                                tBodyAccJerkstdY = mean(tBodyAccJerk.std...Y),
                                tBodyAccJerkstdZ = mean(tBodyAccJerk.std...Z),
                                tBodyGyrostdX = mean(tBodyGyro.std...X),
                                tBodyGyrostdY = mean(tBodyGyro.std...Y),
                                tBodyGyrostdZ = mean(tBodyGyro.std...Z),
                                tBodyGyroJerkstdX = mean(tBodyGyroJerk.std...X),
                                tBodyGyroJerkstdY = mean(tBodyGyroJerk.std...Y),
                                tBodyGyroJerkstdZ = mean(tBodyGyroJerk.std...Z),
                                tBodyAccMagstd = mean(tBodyAccMag.std..),
                                tGravityAccMagstd = mean(tGravityAccMag.std..),
                                tBodyAccJerkMagstd = mean(tBodyAccJerkMag.std..),
                                tBodyGyroMagstd = mean(tBodyGyroMag.std..),
                                tBodyGyroJerkMagstd = mean(tBodyGyroJerkMag.std..),
                                fBodyAccstdX = mean(fBodyAcc.std...X),
                                fBodyAccstdY = mean(fBodyAcc.std...Y),
                                fBodyAccstdZ = mean(fBodyAcc.std...Z),
                                fBodyAccJerkstdX = mean(fBodyAccJerk.std...X),
                                fBodyAccJerkstdY = mean(fBodyAccJerk.std...Y),
                                fBodyAccJerkstdZ = mean(fBodyAccJerk.std...Z),
                                fBodyGyrostdX = mean(fBodyGyro.std...X),
                                fBodyGyrostdY = mean(fBodyGyro.std...Y),
                                fBodyGyrostdZ = mean(fBodyGyro.std...Z),
                                fBodyAccMagstd = mean(fBodyAccMag.std..),
                                fBodyBodyAccJerkMagstd = mean(fBodyBodyAccJerkMag.std..),
                                fBodyBodyGyroMagstd = mean(fBodyBodyGyroMag.std..),
                                fBodyBodyGyroJerkMagstd = mean(fBodyBodyGyroJerkMag.std..))
        
        write.table(final_tidy,
                    "final_tidy_dataset.txt",
                    append = FALSE,
                    quote = FALSE,
                    sep = " ",
                    row.names = FALSE)       
}
