library(reshape2)

cleanfile = "cleanfile.txt"
extracted_feats <- c(1, 2, 3, 4, 5, 6, 41, 42, 43, 44, 45, 46, 81, 82, 83, 84, 85, 86, 121, 122, 123, 124, 125, 126, 161, 162, 163, 164, 165, 166, 201, 202, 214, 215, 227, 228, 240, 241, 253, 254, 266, 267, 268, 269, 270, 271, 345, 346, 347, 348, 349, 350, 424, 425, 426, 427, 428, 429, 503, 504, 516, 517, 529, 530, 542, 543)
extracted_feat_names <- c("tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z", "tBodyAcc-std()-X", "tBodyAcc-std()-Y", "tBodyAcc-std()-Z", "tGravityAcc-mean()-X", "tGravityAcc-mean()-Y", "tGravityAcc-mean()-Z", "tGravityAcc-std()-X", "tGravityAcc-std()-Y", "tGravityAcc-std()-Z", "tBodyAccJerk-mean()-X", "tBodyAccJerk-mean()-Y", "tBodyAccJerk-mean()-Z", "tBodyAccJerk-std()-X", "tBodyAccJerk-std()-Y", "tBodyAccJerk-std()-Z", "tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z", "tBodyGyro-std()-X", "tBodyGyro-std()-Y", "tBodyGyro-std()-Z", "tBodyGyroJerk-mean()-X", "tBodyGyroJerk-mean()-Y", "tBodyGyroJerk-mean()-Z", "tBodyGyroJerk-std()-X", "tBodyGyroJerk-std()-Y", "tBodyGyroJerk-std()-Z", "tBodyAccMag-mean()", "tBodyAccMag-std()", "tGravityAccMag-mean()", "tGravityAccMag-std()", "tBodyAccJerkMag-mean()", "tBodyAccJerkMag-std()", "tBodyGyroMag-mean()", "tBodyGyroMag-std()", "tBodyGyroJerkMag-mean()", "tBodyGyroJerkMag-std()", "fBodyAcc-mean()-X", "fBodyAcc-mean()-Y", "fBodyAcc-mean()-Z", "fBodyAcc-std()-X", "fBodyAcc-std()-Y", "fBodyAcc-std()-Z", "fBodyAccJerk-mean()-X", "fBodyAccJerk-mean()-Y", "fBodyAccJerk-mean()-Z", "fBodyAccJerk-std()-X", "fBodyAccJerk-std()-Y", "fBodyAccJerk-std()-Z", "fBodyGyro-mean()-X", "fBodyGyro-mean()-Y", "fBodyGyro-mean()-Z", "fBodyGyro-std()-X", "fBodyGyro-std()-Y", "fBodyGyro-std()-Z", "fBodyAccMag-mean()", "fBodyAccMag-std()", "fBodyBodyAccJerkMag-mean()", "fBodyBodyAccJerkMag-std()", "fBodyBodyGyroMag-mean()", "fBodyBodyGyroMag-std()", "fBodyBodyGyroJerkMag-mean()", "fBodyBodyGyroJerkMag-std()")

activities <- c(1, 2, 3, 4, 5, 6)
activity_names <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

feat_file <- function(name) {
  paste ("X_", name, ".txt", sep = "")
}

act_file <- function(name) {
  paste ("Y_", name, ".txt", sep = "")
}

sub_file <- function(name) {
  paste ("subject_", name, ".txt", sep = "")
}


get_data <- function(dir, name){
  top_dir <- file.path (dir, name)
  print ("top_dir = ")
  print (top_dir)
  feat_name <- file.path(top_dir, feat_file(name))
  act_name <- file.path(top_dir, act_file(name))
  sub_name <- file.path(top_dir, sub_file(name)) 
  
  print ("Reading features...");
  feat <- read.table(feat_name)[extracted_feats]
  names(feat) <-extracted_feat_names
  
  clean_data <- feat
  
  print ("Reading activities...");
  act <- read.table(act_name)
  names(act) <-c("activity")
  act$activity <- factor(act$activity, levels = activities, labels = activity_names)
  
  clean_data <- cbind(clean_data, activity = act$activity)
  
  print ("Reading subjects...");
  sub <- read.table(sub_name)
  names(sub) <-c("subject")
  
  clean_data <- cbind(clean_data, subject = sub$subject)
  
  #Returns clean data
  clean_data
}

run_analysis <- function(dir) {
	print ("Starting run_analysis...");
  test <- get_data (dir, "test")
	train <- get_data (dir, "train")
  
  print ("Joining datasets...")
  merged_data <- rbind(test, train)
  
  #Reshape the data
  print ("Reshaping the data...")
  merged_data.long <- melt(merged_data, id = c("subject", "activity"))
  merged_data.wide <- dcast(merged_data.long, subject + activity ~ variable, mean)
  
  merged_data.clean <- merged_data.wide

  clean_filename <- file.path (dir, cleanfile)
  print ("Saving cleaned data in:")
  print (clean_filename)
  write.table(merged_data.clean, clean_filename, row.name=F, quote=F)

	print ("Done analysis...");
}