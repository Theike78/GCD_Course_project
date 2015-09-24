# GCD_Course_project
The course project of Getting and Cleaning Data

With the Human Activity Recognition Using Smartphones Dataset Version 1.0 I created a tidy dataset (final_tidy_dataset.txt).
The Human Activity Recognition Using Smartphones Dataset Version 1.0 I used can be found here:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

A full description is available at the site where the data was obtained
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

This tidy dataset contains:
The training and the test sets in one data set.
The average of the measurements on the mean and standard deviation for each measurement for each activity and each subject from the source data. For an explanation on the columns of the tidy dataset take a look at the CODEBOOK.md in this repository.

The dataset can be downloaded from my Getting and Cleaning Data project assignment page in Coursera.
Use this R code to load it into R and explore it how you like:
data <- read.table("final_tidy_dataset.txt", header = TRUE) 

To reproduce the tidy dataset the run_analysis.R script can be used as long as long as the Samsung data is in your working directory.
