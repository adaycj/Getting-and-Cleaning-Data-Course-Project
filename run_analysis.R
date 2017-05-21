## This function is to complete the Week 4 data cleaning project
run_analysis <- function(){
    ## PLEASE NOTE: The steps below are the criteria from the week 4 lesson 
            ##that dictates what this R script should do!
    
    ##Step 1: Merge the training and the test sets to create one data set.
    ##Step 2: Extract only the measurements on the mean and standard deviation for each measurement.
    ##Step 3: Use descriptive activity names to name the activities in the data set
    ##Step 4: Appropriately label the data set with descriptive variable names.
    ##Step 5: From the data set in step 4, creates a second, independent tidy data 
    ##set with the average of each variable for each activity and each subject.
    
    ##prepping R to work - you could call this Step 0 even though it was not listed
            ##in the assignment
    
    ## install pacakges and load the library as needed  
    packages(plyr)
    library(plyr)
    ## set the working directory for the needed files
    setwd("D:/Data Science/Week 4")
    
    ##Step 1: Merge the training and the test sets to create one data set.
    combined_data <- merge_ds(combined_data)
    
    ##Step 2: Extract only the measurements on the mean and standard deviation for each measurement.
    combined_data <- extract_mean_sd(combined_data)
    
    ##Step 3: Use descriptive activity names to name the activities in the data set
    combined_data <- descriptive_names(combined_data)
    
    ##Step 4: Appropriately label the data set with descriptive variable names.
    combined_data <- appropriate_labels(combined_data)
    
    ##Step 5: From the data set in step 4, creates a second, independent tidy data 
    ##set with the average of each variable for each activity and each subject.
    independent_tidy_data(combined_data)
       
}

merge_ds <- function(combined_data){
    ##Step 1: Merge the training and the test sets to create one data set.

    ## read the file with the features  
    ## the features table does not need tidying 
    features <- read.table("./UCI HAR Dataset/features.txt",header=FALSE)
    
    ## read the file with the activity labels
    activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE)
    ## add tidy column names
    colnames(activity_labels)<-c("activityID","activityType")
    
    ## now read files from the /train/ directory
    
    ## read the file with the training subjects
    subject_train <-read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE)
    ## add tidy column names in human readable text
    colnames(subject_train) <- "subjectID"
    
    X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE)
    ## add tidy column names from features
    colnames(X_train) <- features[,2]
    
    y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header=FALSE)
    ## add tidy column names in human readable text
    colnames(y_train) <- "activityID"
    
    ##combine the training data
    trainData <- cbind(y_train,subject_train,X_train)  
    
    ## now read files from the /test/ directory
    
    ## read the file with the test subjects
    subject_test <-read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE)
    ## add tidy column names in human readable text
    colnames(subject_test) <- "subjectID"
    
    X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE)
    ## add tidy column names from features
    colnames(X_test) <- features[,2]
    
    y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header=FALSE)
    ## add tidy column names in human readable text
    colnames(y_test) <- "activityID"
   
    #combine the test data
    testData <- cbind(y_test,subject_test,X_test)
  
    #combine the tidy test and training data
    combined_data <- rbind(trainData,testData)
    return(combined_data)
}

extract_mean_sd <- function(combined_data){
    # 2. Extract only the measurements on the mean and standard deviation for each measurement
    
    ## extract the means standard, deviations with the subject and activity ID
    MeanSDOnly <-combined_data[,grepl("mean|std|subject|activityID",colnames(combined_data))]
    combined_data <- MeanSDOnly
    return(combined_data)
    }

descriptive_names <- function(combined_data){    
    #3. #Use descriptive activity names to name the activities in the data set
    
    ## read the file with the activity labels
    activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE)
    ## add tidy column names
    colnames(activity_labels)<-c("activityID","activityType")
    
    combined_data <- join(combined_data, activity_labels, by = "activityID", match = "first")
    combined_data <- combined_data[,-1]
    return(combined_data)
}
 
appropriate_labels <- function(combined_data){   
    #4. Appropriately label the data set with descriptive variable names.
    
    ## get rid of parentheses
    col_headers(combined_data) <- gsub("\\(|\\)", "", col_headers(combined_data), perl  = TRUE)
    
    ##A syntactically valid name consists of letters, 
    ##numbers and the dot or underline characters and starts with a letter or 
    ##the dot not followed by a number.
    col_headers(combined_data) <- make.col_headers(col_headers(combined_data))
    
    ##Fix abbreviated col_headers
    ##change Mag for Magnitude
    col_headers(combined_data) <- gsub("Mag", "Magnitude", col_headers(combined_data))
    ##change ^t for Magnitude
    col_headers(combined_data) <- gsub("^t", "Time", col_headers(combined_data))
    ##change Freq for Time
    col_headers(combined_data) <- gsub("Freq", "Frequency", col_headers(combined_data))
    ##change Acc for Frequency
    col_headers(combined_data) <- gsub("Acc", "Acceleration", col_headers(combined_data))
    ##change BodyBody for Acceleration
    col_headers(combined_data) <- gsub("BodyBody", "Body", col_headers(combined_data))
    ##change mean for Body
    col_headers(combined_data) <- gsub("mean", "Mean", col_headers(combined_data))
    ##change ^f for Mean
    col_headers(combined_data) <- gsub("^f", "Frequency", col_headers(combined_data))
    ##change std for Frequency
    col_headers(combined_data) <- gsub("std", "Standard", col_headers(combined_data))
    return(combined_data)
}

independent_tidy_data <- function(combined_data){
    ##Step 5: From the data set in step 4, creates a second, independent tidy data 
    ##set with the average of each variable for each activity and each subject.
    
    new_tidy_data <- ddply(combined_data, c("subjectID","activityType"), numcolwise(mean))

    write.csv(new_tidy_data, file="tidy_data.csv", row.names = FALSE)
}

packages<-function(x){
    x<-as.character(match.call()[[2]])
    if (!require(x,character.only=TRUE)){
        install.packages(pkgs=x,repos="http://cran.r-project.org")
        require(x,character.only=TRUE)
    }
}