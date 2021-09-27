##calling the "dplyr" library
library(dplyr)

#### Item 0 ####
#upload data and informatation to R environment 
features <- read.table("UCI HAR Dataset/features.txt", header = F, sep = " ")
colnames(features) <- c("n", "functions")

activities <- read.table("UCI HAR Dataset/activity_labels.txt", header = F, sep = " " )
colnames(activities) <- c("code", "activity")

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = F)
colnames(subject_test) <- "subject"

x_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = F)
colnames(x_test) <- features$functions

y_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = F)
colnames(y_test) <- "code"

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",header = F)
colnames(subject_train) <- "subject"

x_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = F)
colnames(x_train) <- features$functions

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = F)
colnames(y_train) <- "code"

#### Item 1 ####
#1. Merges the training and the test sets to create one data set.

#because of test and train have the same columns
### join rows from subject_test and subject_train
subject <- rbind(subject_test,subject_train)
### join rows from x_test and x_train
x <- rbind(x_test, x_train)
### join rows from y_test and y_train
y <- rbind(y_test, y_train)

#checking row number to be able add the three data sets into new one 
nrow(subject)
nrow(x)
nrow(y)

#bind columns because have the same rows number
mdata <- cbind(subject, x, y)

##### End of item 1

#### Item 2 ####
#Extracts only the measurements on the mean and standard deviation for each measurement.
#For this item I assume and  select for columns subject and those have "mean" and "std" values in names
tdata <- mdata %>% select(subject, code, contains("mean"), contains("std"))


##### item 3 #########
#Uses descriptive activity names to name the activities in the data set
#update code column with the activities' names in column 2 of activities
tdata$code <- activities[tdata$code, 2]

#### item 4 #######
#Appropriately labels the data set with descriptive variable names.

#check the colnames to identify items to update, this colnames instructions will be appear for each update
colnames(tdata)

#update the t for Time and f for Frequency at the begin of name
names(tdata)<-gsub("^t", "Time", names(tdata))
names(tdata)<-gsub("^f", "Frequency", names(tdata))
colnames(tdata)

#update for capital letter first letter on name
names(tdata)[2] = "Activities"
names(tdata)[1] = "Subject"
colnames(tdata)

#update for mean for Mean and std for Std to have a first letter capital standar
names(tdata)<-gsub("-mean()", "Mean", names(tdata), ignore.case = TRUE)
names(tdata)<-gsub("-std()", "Std", names(tdata), ignore.case = TRUE)
colnames(tdata)

#update to remove the parenthesis because it appears a lot
names(tdata)<-gsub("\\(\\)", "", names(tdata))
colnames(tdata)

#update Mag to Magnitude
names(tdata)<-gsub("Mag", "Magnitude", names(tdata))
colnames(tdata)

#there are some tBody texts that changed to TimeBody
names(tdata)<-gsub("\\(tBody", "\\(TimeBody", names(tdata))
colnames(tdata)

#update Freq to Frequency
names(tdata)<-gsub("Freq", "Frequency", names(tdata), ignore.case = TRUE)
colnames(tdata)

#update BodyBody to Body, I assume that it should be one time word
names(tdata)<-gsub("BodyBody", "Body", names(tdata))
colnames(tdata)

#update Acc to Accelerometer
names(tdata)<-gsub("Acc", "Accelerometer", names(tdata))
colnames(tdata)

#update Gyro to Gyroscope
names(tdata)<-gsub("Gyro", "Gyroscope", names(tdata))
colnames(tdata)

#update angle to Angle, just to have the first capital letter standar
names(tdata)<-gsub("angle", "Angle", names(tdata))
colnames(tdata)

#update gravityMean to GravityMean, just to have the first capital letter standar
names(tdata)<-gsub("gravityMean", "GravityMean", names(tdata))
colnames(tdata)

#update gravity to Gravity, just to have the first capital letter standar
names(tdata)<-gsub("gravity", "Gravity", names(tdata))
colnames(tdata)


### item 5 ######
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#save result dataframe to independent data frame
independent <- tdata %>%
  group_by(Subject, Activities) %>%
  summarise_all(list(mean=mean))

##View independent result
View(independent)

#write the result in an TXT file as it request
write.table(independent, "independent.txt", row.name=FALSE)

#End of file


