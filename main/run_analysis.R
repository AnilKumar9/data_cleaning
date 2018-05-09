##Downloading and Unzipping data
#Creating a new directory to store the data 
dir.create("cleaning_exercies")

setwd("cleaning_exercies")

#Downlaoding dataset from web
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","zipped_file")

#Unzipping the data set
zipped_file<-unzip("zipped_file",exdir="unzipped_data")

#Changing directory to read unzipped data
setwd("unzipped_data")
data_folder<-list.files()
setwd(data_folder)

################################Step 1(Combining data sets)######################################

#reading trained dataset

setwd("./train")

data_set<-read.table("X_train.txt", header = FALSE)
train_activities<-read.table("y_train.txt",header=FALSE,col.names=c("activity_id"))
train_subjects<-read.table("subject_train.txt",header=FALSE,col.names=c("subject_id"))
train_set<-cbind(train_subjects,train_activities,data_set)

#reading test data set

setwd('..')
setwd("./test")

data_set2<-read.table("X_test.txt", header = FALSE)
test_activities<-read.table("y_test.txt",header=FALSE,col.names=c("activity_id"))
test_subjects<-read.table("subject_test.txt",header=FALSE,col.names=c("subject_id"))
test_set<-cbind(test_subjects,test_activities,data_set2)
#Combing both the data sets
data_set3<-rbind(test_set,train_set)

########################Step-2(measurements on the mean and standard deviation )##############################################

setwd('..')

#reading features names
features_names<-read.table("features.txt",header = FALSE)
features_names<-features_names[,c(2)]
#Selecting features that are realted to mean or standard deviation
Importnat_column_numbers<-grep(("mean|std"),tolower(features_names))
Importnat_column_numbers_names<-grep(("mean|std"),tolower(features_names),value = TRUE)
#Making sure that column numbersare in line with own dataset
Importnat_column_numbers2<-c(c(1,2),Importnat_column_numbers+2)
#Subseting data set for measurements on the mean and standard deviation 
data_set4<-data_set3[,Importnat_column_numbers2]

########################Step-3(descriptive activity names to name the activities)##############################################

#reading activties names
activities<-read.table("activity_labels.txt",header = FALSE,stringsAsFactors = FALSE)
colnames(activities)<-c("activity_id","activity_names")

#Using descriptive activity name

train_activities_names<-merge(activities,data_set4,by.x ="activity_id",by.y="activity_id")
train_activities_names<-train_activities_names[,c(2:ncol(train_activities_names))]


########################Step-4(Appropriately labeling data set)###################################

colnames(train_activities_names)<-c(c("activity_names","subject_id"),Importnat_column_numbers_names)


########################Step-4(Appropriately labels the data se)t###################################

#Cleaning column names as they have some strings like () etc..
library(dplyr)

colnames(train_activities_names)<-gsub("\\(\\)", "", gsub("-","_",colnames(train_activities_names)))

#getting mean for all activities and subjects
data_set6<-train_activities_names %>%
  group_by(activity_names, subject_id) %>%
  summarize_all( mean)

#making empty data frame for the final data

data_set7 <- data.frame(matrix(ncol = 4, nrow = 0),stringsAsFactors=FALSE)
colnames(data_set7)<-c("Activity","Subject_id","variable","mean_val")


#Applying for loop against all the column names to get data in desired format
for (a in (3:ncol(data_set6)))
{
  data_set8<-data_set6[,c(1,2,a)]
  data_set8$newcol <- rep(colnames(data_set6)[a],nrow(data_set8))
  colnames(data_set8)<-c("Activity","Subject_id","variable","mean_val")
  data_set8[,c(1,2,3,4)]<-data_set8[,c(1,2,4,3)]
  data_set7<-rbind(data.frame(data_set7,stringsAsFactors=FALSE),data.frame(data_set8,stringsAsFactors=FALSE))
}

setwd('..')

#Copying the data set to local

write.table(data_set7, file = "final_data.txt",sep="\t", row.names = FALSE)

#data_set7 is the independent tidy data set with the average of each variable 
#for each activity and each subject.




