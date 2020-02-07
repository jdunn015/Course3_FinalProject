
library(data.table)
library(plyr)
library(dplyr)
library(stringr)

#Download the zipfile containing the data


filename <-"Course3_FinalProject.zip"

if (!file.exists(filename)){
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileurl, filename,mode="wb")
}

data<-"UCI HAR Dataset"
if (!file.exists(data)) {
  unzip(filename)
}


#Load subjects ids for test and training data, loads test numbers corresponding to activitiy labels.

subject_test <- read.delim("UCI HAR Dataset/test/subject_test.txt",col.names="subject",header=FALSE)
subject_train<- read.delim("UCI HAR Dataset/train/subject_train.txt",col.names="subject",header=FALSE)
test_numbers<- read.delim("UCI HAR Dataset/test/y_test.txt", col.names="id",header=FALSE)
train_numbers<-read.delim("UCI HAR Dataset/train/y_train.txt",col.names="id",header=FALSE)
activity_labels<-read.delim("UCI HAR Dataset/activity_labels.txt",col.names=c("id"),header=FALSE)

#Splits activity_labels data frame into two columns.  When split data frame is converted to a matrix so made it back into a data frame and assinged column headers.
activity_labels<-str_split_fixed(activity_labels$id, " ", 2)
x<-c("id","activity")
colnames(activity_labels)<-x
activity_labels<-data.frame(activity_labels)

#Used join to merge activity_labels to test_numbers in order to assign the activity to each test number.
test_labels<-join(test_numbers,activity_labels,by="id")
train_labels<-join(train_numbers,activity_labels,by="id")

#column binded the test and train label data frames to corresponding subject data frames and renamed the second column to activity.
test_activities<-cbind(subject_test,test_labels[,2])
train_activities<-cbind(subject_train,train_labels[,2])
test_activities<-dplyr::rename(test_activities,activity= names(test_activities)[2])
train_activities<-dplyr::rename(train_activities,activity=names(train_activities)[2])

#read in the features file and remove () from the values for aesthetics
features<-read.delim(file="UCI HAR Dataset/features.txt",header=FALSE)
features<-as.character(features[,1])
features<-gsub('[()]', '',features)

# read in the test and training data sets and assign features from above to the columnnames.
X_test<-read.table(file="UCI HAR Dataset/test/X_test.txt", header=FALSE, col.names=features,colClasses="numeric",stringsAsFactors = FALSE)
X_train<-read.table(file="UCI HAR Dataset/train/X_train.txt", header=FALSE, col.names=features,colClasses="numeric",stringsAsFactors = FALSE)

#Columnbind the test and training data sets to their corresponding subject/activity data frames created previously.

X_test2<-cbind(test_activities,X_test)
X_train2<-cbind(train_activities,X_train)

# Rowbind the test and training data sets

Test_Train<-rbind(X_test2,X_train2)

#Select the columns which have "mean" or "std" in the column header.  Note ignore.case=TRUE to account for non lower case values.

namesfilter<-grep("mean|std",names(Test_Train), ignore.case=TRUE,value=TRUE)
Test_Train_mean_std<-Test_Train %>% select(subject,activity,namesfilter)
Test_Train_mean_std$subject<-as.factor(Test_Train_mean_std$subject)

#Grouped data by subject and activity and calculated the mean of each variable.
Melted_Data<-reshape2::melt(Test_Train_mean_std,id=c("subject","activity"))
Subject_Activity_Means<-reshape2::dcast(Melted_Data,subject+activity ~ variable,mean)

#Write data to tidydata.txt file
write.table(Subject_Activity_Means,"tidydata.txt",row.name=FALSE)
