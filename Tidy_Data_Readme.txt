## run_analysis.R

## summary of required instructions:
##      1. Merge the training and test data sets to create on data set
##      2. Extract only the measurements on the mean and standard deviation for each measurement
##      3. Use descriptive activity names to name the activities in the data set
##      4. Appropriately labels the data set with the descriptive variable names
##      5. From the data set in step 4, create a second, independent tidy data set with the average
##         of each variable for each activity and each subject

submit <- local( {
        
        # Download the data and place into the proper directory - 
        # the next step is to unzip the downloaded data file set - using its default folder structure
        # This also assumes that the folder is under the current working directory
        # Make sure the assumed working directory exists - if not create it
        
        if(!file.exists("./data")){dir.create("./data")}
        
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        
        download.file(fileURL, destfile = "./data/UCI HAR Dataset.zip", method = "curl")
        
        unzip("./data/UCI HAR Dataset.zip", exdir = "./data")
        
        ## now, read in each data file 
        
        ## First my summary of what each file contains
        ## X-train is 7352 rows by 561 columns
        ## Y-train is 7352 rows by 1 column
        ## Subject Train is 7352 rows by 1 column
        ## X-test is 2947 rows by 561 columns
        ## Y-test is 2947 rows by 1 column
        ## Subject Test is 2947 rows by 1 column
        ## Activity Labels is 6 rows by 1 column, these correspond to the Y test/train files
        ## Features are 561 rows by 1 column. These are the variable names for the X test/train files
        ## Feature Info file contains explanative data for the feature set
        
        xTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt", header = FALSE)
        yTrain <- read.table("./data/UCI HAR Dataset/train/Y_train.txt", header = FALSE)
        xTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt", header = FALSE)
        yTest <- read.table("./data/UCI HAR Dataset/test/Y_test.txt", header = FALSE)
        subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
        subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
        activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE)
        features <- read.table("./data/UCI HAR Dataset/features.txt", header = FALSE)
        
        
        ## merge the X train and X test data sets
        
        mergedX <- rbind(xTrain, xTest)
        
        ## merge the Y train and Y test data sets
        
        mergedY <- rbind(yTrain, yTest)
        
        ## merge the subjectTrain and subjectTest data sets
        
        mergedSubject <- rbind(subjectTrain, subjectTest)
        
        ## get the variable names from the Features data set and make them the column names of
        ## the mergedX data set
        
        colnames(mergedX) <- features[, 2]
         
        ## subset on measurements of the mean and standard deviation
        ## first use grep to pull the column indexes for each
        
        mn <- grep("mean", features$V2)
        std <- grep("std", features$V2)
        
        ## create the clean data set with the mean and standard deviation columns
        
        mergedXSub <- mergedX[, c(mn, std)]
      
                
       ## now to marry up the activity labels into from the mergedY and activityLabels data sets
       ## create a temp since I may screw up my good data. Replace the numeric value in column 1 
       ## of mY with the text label from column 2 of activity label
        
        mY <- mergedY
        mY[, 1] = activityLabels[mY[,1],2]
        
        ## Make some easier to understand column labels in the final two data sets. Other than
        ## V1

        names(mY) <- "Activity"
        names(mergedSubject) <- "Subject_Code"
        
        ## Finally put all the data sets togehter and write it out
        
        outFile <- cbind(mergedSubject, mergedXSub, mY)
        write.table(outFile, "Meehan_tidy_data_set.txt")
        
        ## The means for each of the columns except Subject Code and Activity
      
        finalOut <- ddply(outFile, .(Subject_Code, Activity), .fun=function(x) {colMeans(outFile[ , -c(1, 81)])})
        write.table(finalOut, "Meehan_tidy_data_means_set.txt", row.names = FALSE, col.names = TRUE)

        })