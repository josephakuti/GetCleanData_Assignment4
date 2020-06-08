Below is the read-me file that explains the analysis to create the tidy dataset:

### (1) Merge the training and the test sets to create one data set. ###

# Ensure a data folder is available

if (!file.exists("data")) {
        dir.create("data")
}

# Download zip file to destination file location - working directory

dest_file <- "data/data.zip"
urlziplocation <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(urlziplocation, dest_file)


# Store the test and training data in a variable

xtest <- unz(dest_file, "UCI HAR Dataset/test/X_test.txt") #TEMP object
x_test <- read.table(xtest, quote="\"", comment.char="")
xtrain <- unz(dest_file, "UCI HAR Dataset/train/X_train.txt") #TEMP object
x_train <- read.table(xtrain, quote="\"", comment.char="")

ytest <- unz(dest_file, "UCI HAR Dataset/test/y_test.txt") #TEMP object
y_test <- read.table(ytest, quote="\"", comment.char="")
ytrain <- unz(dest_file, "UCI HAR Dataset/train/y_train.txt") #TEMP object
y_train <- read.table(ytrain, quote="\"", comment.char="")

subjecttest <- unz(dest_file, "UCI HAR Dataset/test/subject_test.txt") #TEMP object
subject_test <- read.table(subjecttest, quote="\"", comment.char="")
subjecttrain <- unz(dest_file, "UCI HAR Dataset/train/subject_train.txt") #TEMP object
subject_train <- read.table(subjecttrain, quote="\"", comment.char="")

# Store the feature labels in a variable

features <- unz(dest_file, "UCI HAR Dataset/features.txt") #TEMP object
features_labels <- read.table(features, quote="\"", comment.char="")

features_labels <- features_labels[,-1]
features_labels <- as.character(features_labels) #Convert factor to character
features_labels <- append("Subject", features_labels) #Add Subject label
features_labels <- append(features_labels, "Activity") #Add Activity label

#Combine test and training data with column names

test <- cbind(subject_test, x_test, y_test) #Test dataset
train <- cbind(subject_train, x_train, y_train) #Training dataset
data <- rbind(test, train)




### 2. Extract only the measurements on the mean and standard deviation for each measurement.


features_labels <- tolower(features_labels) #Convert labels to lowercase

mean_loc <- grep("mean", features_labels) #Identify the location of all mean variables
sd_loc <- grep("std", features_labels) #Identify the location of all std variables
loc_feature <- c(mean_loc, sd_loc) #Create a vector with locations of mean/std variables

feature_list <- features_labels[loc_feature] #Filter only mean/std labels
data_filter <- data[,loc_feature] #Filter only mean/std labels

subject <- rbind(subject_test, subject_train) #Combine test/train subject data
y <- rbind(y_test, y_train) #Combine test/train activity data

data_filter <- cbind(data_filter, y) #Add the activity data to the mean/std data (data_filter)
data_filter <- cbind(subject, data_filter) #Add the subject data to the mean/std data (data_filter)

feature_list <- append("subject", feature_list)
feature_list <- append(feature_list, "activity")

colnames(data_filter) <- feature_list #Add label names to the mean/std data

### 3. Use descriptive activity names to name the activities in the data set

data_filter$Activity[data_filter$Activity==1] <- "WALKING"
data_filter$Activity[data_filter$Activity==2] <- "WALKING_UPSTAIRS"
data_filter$Activity[data_filter$Activity==3] <- "WALKING_DOWNSTAIRS"
data_filter$Activity[data_filter$Activity==4] <- "SITTING"
data_filter$Activity[data_filter$Activity==5] <- "STANDING"
data_filter$Activity[data_filter$Activity==6] <- "LAYING"


### 4. Appropriately labels the data set with descriptive variable names.

clean_names <- sub("angle", "angular-velocity", feature_list)
clean_names <- sub("tbodyacc", "time-body-acceleration-signals", clean_names)
clean_names <- sub("tgravityacc", "time-gravity-acceleration-signals", clean_names)
clean_names <- sub("tgravityacc", "time-gravity-acceleration-signals", clean_names)
clean_names <- sub("fbodyacc", "frequency-body-acceleration-signals", clean_names)
clean_names <- sub("tbodygyro", "time-body-acceleration-signals-gyroscope", clean_names)
clean_names <- sub("tbodygyro", "time-body-acceleration-signals-gyroscope", clean_names)
clean_names <- sub("fbodygyro", "frequency-body-signals-gyroscope", clean_names)
clean_names <- sub("fbodybodyacc", "frequency-body-acceleration-signals", clean_names)
clean_names <- sub("fbodybodygyro", "frequency-body-signals-gyroscope", clean_names)
clean_names <- sub("jerkmag", "-jerkmagnitude", clean_names)
clean_names <- sub("jerk-", "-jerksignal-", clean_names)
clean_names <- sub("mag-", "-magnitude-", clean_names)
colnames(data_filter) <- clean_names

### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

write.table(data_filter, file = "tidy_data.txt")

