## Download train data set
X_train<-read.table("/Users/kavithareddydwaram/Desktop/UCI HAR Dataset/train/X_train.txt",header=FALSE,sep="",stringsAsFactors = FALSE,quote = "",dec=".")
subject_train<-read.table("/Users/kavithareddydwaram/Desktop/UCI HAR Dataset/train/subject_train.txt",header=FALSE,sep="",stringsAsFactors = FALSE,quote = "")
colnames(subject_train)<-c("subject")
y_train<-read.table("/Users/kavithareddydwaram/Desktop/UCI HAR Dataset/train/y_train.txt",header=FALSE,sep="",stringsAsFactors = FALSE,quote = "")
colnames(y_train)<-c("y")
trainPartial<-cbind(X_train,subject_train)
trainDataComplete<-cbind(trainPartial,y_train)
View(trainDataComplete)

## Download test data set
X_test<-read.table("/Users/kavithareddydwaram/Desktop/UCI HAR Dataset/test/X_test.txt",header=FALSE,sep="",stringsAsFactors = FALSE,quote = "",dec=".")
subject_test<-read.table("/Users/kavithareddydwaram/Desktop/UCI HAR Dataset/test/subject_test.txt",header=FALSE,sep="",stringsAsFactors = FALSE,quote = "")
colnames(subject_test)<-c("subject")
y_test<-read.table("/Users/kavithareddydwaram/Desktop/UCI HAR Dataset/test/y_test.txt",header=FALSE,sep="",stringsAsFactors = FALSE,quote = "")
colnames(y_test)<-c("y")
testPartial<-cbind(X_test,subject_test)
testDataComplete<-cbind(testPartial,y_test)
View(testDataComplete)

## Read features titles
titles<-read.table("/Users/kavithareddydwaram/Desktop/UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)
titles_vect<-as.vector(titles)
titles_vect<-titles_vect[,2]
## colnames(trainDataComplete)<-titles_vector 
titlesVector<-c(titles_vect,"subject","y")
##colnames(testDataComplete)<-titlesVectorTest

## Merge train and test data sets to create one merged data set
mergedData<-rbind(testDataComplete,trainDataComplete)
View(mergedData)
mergedDataTbl<-tbl_df(mergedData)
## Add titles to merged Data
colnames(mergedDataTbl)<-titlesVector

## Retrieve unique feature columns
mergedDataTblUnique<-mergedDataTbl[,!duplicated(colnames(mergedDataTbl))]
uniqueVariables<-colnames(mergedDataTblUnique)
uniqueVariables

## Extract  only the features with mean and standard deviation
meanVariables<-grep("mean()",uniqueVariables,value=TRUE)
meanVariables

stdVariables<-grep("std()",uniqueVariables,value=TRUE)
stdVariables

## Extract  only the mean and standard deviation measurements for each measurement
meanStdData<-select(mergedDataTblUnique,subject,y,meanVariables,stdVariables)
View(meanStdData)

## Descriptive activity names
activityLabels<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")

## Name the activities in the data set with descriptive activity names
f<-function(x){
  gsub(x,activityLabels[x],x)
}
activity<-sapply(meanStdData$y,f)

meanStdDataActivityLabels<-cbind(meanStdData,activity)
tidyingData<-select(meanStdDataActivityLabels,-y)

## Group the tidying data set with activity and subject
bySubjectActivity<-group_by(tidyingData,subject,activity)

## Label the data set with descriptive variable names
colnames(bySubjectActivity)<-gsub("^t","time",colnames(bySubjectActivity))
colnames(bySubjectActivity)<-gsub("^f","frequency",colnames(bySubjectActivity))
colnames(bySubjectActivity)<-gsub("mean","Mean",colnames(bySubjectActivity))
colnames(bySubjectActivity)<-gsub("^std","Std",colnames(bySubjectActivity))
colnames(bySubjectActivity)<-gsub("-","",colnames(bySubjectActivity))
colnames(bySubjectActivity)<-gsub("\\()","",colnames(bySubjectActivity))

## Create an independent tidy data set with the average of each variable for each activity and subject
tidyData<- bySubjectActivity %>% summarise_all(mean)
View(tidyData)

