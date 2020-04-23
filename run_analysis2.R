##dplyr packaged needed for this function.
##creates directory within whatever WD user is in, downloads data to new directory, output goes
##to that directory as well.

run_analysis <- function() {
  if(!dir.exists("run_analysis")) {
    dir.create("run_analysis")
  }
  setwd("run_analysis")
  ##create run_analysis directory, then set as the wd
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              method = "curl",  destfile = "data.zip")
  data <- unzip("data.zip")

  testid <- tbl_df(read.table(data[14])) ##test/subject_test.txt
  activityid_test <- tbl_df(read.table(data[16]))  ##test/y_test.txt
  activityid_test <- mutate(activityid_test, V1 = replace(V1, V1 == 1, "Walking"),
                            V1 = replace(V1, V1 == 2, "Walking_Upstairs"), V1 = replace(V1, V1 == 3, "Walking_Downstairs"),
                            V1 = replace(V1, V1 == 4, "Sitting"), V1 = replace(V1, V1 == 5, "Standing"), V1 = replace(V1, V1 == 6, "Laying"))
  testvalue <- tbl_df(read.table(data[15]))  ##test/X_test.txt
  
  trainid <- tbl_df(read.table(data[26]))  ##train/subject_train.txt
  activityid_train <- tbl_df(read.table(data[28]))  ##train/y_train.txt
  activityid_train <- mutate(activityid_train, V1 = replace(V1, V1 == 1, "Walking"),
                            V1 = replace(V1, V1 == 2, "Walking_Upstairs"), V1 = replace(V1, V1 == 3, "Walking_Downstairs"),
                            V1 = replace(V1, V1 == 4, "Sitting"), V1 = replace(V1, V1 == 5, "Standing"), V1 = replace(V1, V1 == 6, "Laying"))
  trainvalue <- tbl_df(read.table(data[27]))  ##train/X_test.txt
  
  features <- tbl_df(read.table(data[2]))  ##features.txt
  feat_names <- as.character(t(features[,2]))
  
  activity_labels <- tbl_df(read.table(data[1]))  ##activity_labels.txt
 
  testing <- bind_cols(testid, activityid_test, testvalue)
    ##creates testing dataset, unlabelled 
  testing <- mutate(testing, "Group" <- rep("test"))
     ##adds column indicating this is the testing group
  names(testing) <- c("SubjectID", "Activity", feat_names, "Group")
  training <- bind_cols(trainid, activityid_train, trainvalue)
    ##creates training dataset, unlabelled
  training <- mutate(training, "Group" <- rep("train"))
  names(training) <- c("SubjectID", "Activity", feat_names, "Group")
  compiled <- rbind(testing, training)
    ##combines both testing and training dataset, includes labels
  compiled <- compiled[-(c(305:346, 384:425, 463:504))] 
     ##removes all unneeded columns with duplicated variable names - unnecessary for assignment
  extracted <- select(compiled, SubjectID, Activity, matches(c(".*mean", ".*std")))
      ##extracts columns with either mean or std calculations
  grouped <- group_by(extracted, SubjectID, Activity)
  summarized <- summarize_at(grouped, vars(matches("mean|std")),mean)
  write.table(summarized, file = "Wearable_Data.txt")
}



