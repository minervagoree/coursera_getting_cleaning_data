#####Author: Minerva Goree Date:7/17/2014
#####Getting and Cleaning Data course Project given by Coursera

#Step 1: Original data can be found at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#Read in train and test data to have it handy for data manipulation.
myURL <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(myURL,"./ProjectData.zip") #downloads the file into my local computer directory
Unlink(myURL) #Close myURL connection
#Now with the data in my local computer directory I can read data into R
testData <- read.table("~\\X_test.txt", header=FALSE)
trainData <- read.table("~\\X_train.txt", header=FALSE)

#Step 2: Create subject data frame with subjects used in the testing set data.  We will build the data with subjectID, ActivityID, and Activity
##First we read in the data we are going to need:
##We need the subject_test.txt, y_test.txt, and activity_labels.txt data from the original data set
subjectTest <- read.table("~\\subject_test.txt", header=FALSE)
activityTest <- read.table("~\\y_test.txt", header=FALSE)
activityLabels <- read.table("~\\activity_labels.txt", header=FALSE)
##Now we have the data we need.  We will now change the first column name in subjectTest from V1 to SubjectID
v1 <- "subjectID" #assign the column title to a variable
names(subjectTest) <- v1 #assign the variable of our title to change the name in our data set.
##We now have prepped the subjectTest data set.  
##Now add the columns from activityTest and label each activity ID with the actual activity it corresponds to.
##Add the activityID to our subjectTest data frame
subjectTest$activityID <- (activityTest$V1)
##Clean up Activity from uppercase and underscore to lowercase and space between word
a <- sapply(activityLabels, is.factor) #assign all factor columns to a
activityLabels[a] <- lapply(activityLabels[a],as.character) #change all factor columns to string
activityLabels$V2 <- tolower(activityLabels$V2) #make all labels lowercase
activityLabels$V2 <- sub("\\_"," ",activityLabels$V2) #remove the underscore for a space
activityLabels[a] <- lapply(activityLabels[a],as.factor)#change the labels back to factors
##create an activity column in trainLabel data frame matching the activityID to the right activity
subjectTest$activity <- factor(subjectTest$activityID,labels=activityLabels$V2)

#Step 3: Create subject data from with subjects used in the training set data.  We will build the data with subjectID, ActivityID, and Activity
##First we read in the data we are going to need:
##We need the subject_test.txt and y_test.txt data from the original data set. We already have the activityLabels data.
subjectTrain <- read.table("~\\subject_test.txt", header=FALSE)
activityTrain <- read.table("~\\y_test.txt", header=FALSE)
##Now we have the data we need.  We will now change the first column name in subjectTrain from V1 to SubjectID
v2 <- "subjectID" #assign the column title to a variable
names(subjectTrain) <- v2 #assign the variable of our title to change the name in our data set.
##We now have prepped the subjectTrain data set.  
##Now add the columns from activityTrain and label each activity ID with the actual activity it corresponds to.
##Add the activityID to our subjectTrain data frame
subjectTrain$activityID <- (activityTrain$V1)
##create an activity column in trainLabel data frame matching the activityID to the right activity
subjectTrain$activity <- factor(subjectTrain$activityID,labels=activityLabels$V2)

#Step 4: Merge the Subject Train and Test data
subjectInfo <- rbind(subjectTest,subjectTrain)

#Step 5: Clean up the features variables to Camel style to be compliant with R naming convention.
##Read in the features.txt data to be available for manipulation.
features <- read.table("~\\features.txt", header=FALSE)
##Change features from a factor into strings so we can change the variable names to 
i <- sapply(features, is.factor) #assigns factor columns to a variable
features[i] <- lapply(features[i],as.character) #changes all factor columns to string
##clean up features list before adding header to the data
##Note: the \\ is used to escape the characters that might mean something else in R.
features$V2 <- sub("mean","Mean",features$V2)
features$V2 <- sub("std","Std",features$V2)
features$V2 <- sub("mad","Mad",features$V2)
features$V2 <- sub("sma","Sma",features$V2)
features$V2 <- sub("energy","Energy",features$V2)
features$V2 <- sub("iqr","Iqr",features$V2)
features$V2 <- sub("max","Max",features$V2)
features$V2 <- sub("min","Min",features$V2)
features$V2 <- sub("entropy","Entropy",features$V2)
features$V2 <- sub("arCoeff","ArCoeff",features$V2)
features$V2 <- sub("correlation","Correlation",features$V2)
features$V2 <- sub("skewness","Skewness",features$V2)
features$V2 <- sub("kurtosis","Kurtosis",features$V2)
features$V2 <- sub("bands","Bands",features$V2)
features$V2 <- sub("gravity","Gravity",features$V2)
features$V2 <- sub("\\()","",features$V2)
features$V2 <- sub("-","",features$V2)
features$V2 <- sub(",","And",features$V2)
features$V2 <- sub("\\(","Of",features$V2)
features$V2 <- gsub("\\)","",features$V2)
#delete first column in features since we only need column 2 with the variable names
features2 <- (features$V2)

#Step 6: Merge test and train data.
phoneData <- rbind(testData,trainData)

#Step 7: Add feature labels to our phoneData. 
names(phoneData) <- features2 #assigns the variable name to phoneData using features2.

#Step 8: Extract columns that contain data for standard deviation (Std) and Mean. 
##Exclude any data which contains calculations using two or more variables.
##Exclude data that doesn't make sense like BodyBody variables.
phoneData2 <- phoneData[,grepl("Subject|Activity|Mean|Std",names(phoneData))]
phoneData2 <- phoneData2[,-grep("angle|Freq",names(phoneData2))]

#Step 9: Merge the subject data with the phone data
phoneData2 <- cbind(subjectInfo,phoneData2)

#Step 10: First Tidy Data set
write.table(phoneData2,"phoneDataTidy1",row.names=FALSE)

#Step 11: Create a second tidy data set with only subjectID, Activity, and average of each activity and each subject.
aggPhoneData <- aggregate(phoneData2,by=list(phoneData2$subjectID,phoneData2$activity),FUN=mean, na.rm=TRUE)
aggPhoneData <- aggPhoneData[,-grep("subject|activity",names(aggPhoneData))]
colnames(aggPhoneData)[1] <- "SubjectID"
colnames(aggPhoneData)[2] <- "Activity"

#Step 12: Save a file copy of your final tidy data set in csv and txt
write.csv(aggPhoneData,"phoneTidyData2.csv",row.names=FALSE)
write.table(aggPhoneData,"phoneTidyData2.txt",row.names=FALSE)



