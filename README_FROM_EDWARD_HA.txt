  
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
 ======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

The dataset includes the following files:
*Code Book that describes the variables.
*Readme from Edward Ha.



The script below will allow you to answer the Course Project answers.
The path will need to be change to accomodate your own file path.




#####################################################COURSE PROJECT##################################################################################
  #Read XTRAIN  MAC
  #Xtrain<-read.table("C/Documents/COURSERA/CLEANINGDATA/WEEK3_prime/train/X_train.txt",header=FALSE,sep="")
  #train<-read.table("./Documents/COURSERA/CLEANINGDATA/WEEK3_prime/train/y_train.txt",header=FALSE,sep="")
  #subject_train<-read.table("./Documents/COURSERA/CLEANINGDATA/WEEK3_prime/train/subject_train.txt",header=FALSE,sep="")
  library(downloader)
  library(plyr)
  library(dplyr)
  library(Hmisc) 
  library(reshape)
  
  
  
  
  ##    QUESTION 1:  Merges the training and the test sets to create one data set.
  #Header
  Feature<-read.table("C:/Users/edha/Desktop/Documents/Documents/COURSERA/COURSE3-GETTING_AND_CLEANING_DATA/WEEK3/UCI_HAR_Dataset/features.txt",header=FALSE,sep="")
  
  #Read XTRAIN  PC
  Xtrain<-read.table("C:/Users/edha/Desktop/Documents/Documents/COURSERA/COURSE3-GETTING_AND_CLEANING_DATA/WEEK3/UCI_HAR_Dataset/train/X_train.txt",header=FALSE,col.names=Feature[,2],sep="")
  ytrain<-read.table("C:/Users/edha/Desktop/Documents/Documents/COURSERA/COURSE3-GETTING_AND_CLEANING_DATA/WEEK3/UCI_HAR_Dataset/train/y_train.txt",header=FALSE,col.names="Activity",sep="")
  subject_train<-read.table("C:/Users/edha/Desktop/Documents/Documents/COURSERA/COURSE3-GETTING_AND_CLEANING_DATA/WEEK3/UCI_HAR_Dataset/train/subject_train.txt",col.names="Subject",header=FALSE,sep="")
  
  #Read XTEST PC
  Xtest<-read.table("C:/Users/edha/Desktop/Documents/Documents/COURSERA/COURSE3-GETTING_AND_CLEANING_DATA/WEEK3/UCI_HAR_Dataset/test/X_test.txt",header=FALSE,col.names=Feature[,2],sep="")
  ytest<-read.table("C:/Users/edha/Desktop/Documents/Documents/COURSERA/COURSE3-GETTING_AND_CLEANING_DATA/WEEK3/UCI_HAR_Dataset/test/y_test.txt",header=FALSE,col.names="Activity",sep="")
  subject_test<-read.table("C:/Users/edha/Desktop/Documents/Documents/COURSERA/COURSE3-GETTING_AND_CLEANING_DATA/WEEK3/UCI_HAR_Dataset/test/subject_test.txt",header=FALSE,col.names="Subject",sep="")
  
  #Merges Xtrain and subject_train
  MERGE1<-cbind(Xtrain,ytrain,subject_train)
  
  #Merges Xtest and subject_test
  MERGE2<-cbind(Xtest,ytest,subject_test)
   
  #Combines MERGE1 and MERGE2
  MERGE3<-rbind(MERGE1,MERGE2)
  
  MERGEDATA<-tbl_df(MERGE3)
  
    
  ###    QUESTION 2:  Extracts only the measurements on the mean and standard deviation for each measurement. 
  
  MERGEDATA2<-MERGEDATA%>%select (contains("mean"),contains("Subject"),contains("Activity"))
  MERGEDATA3<-MERGEDATA2%>%select (contains("fBodyBodyAcc"),contains("fBodyBodyGyro"),contains("Subject"),contains("Activity"))
  MERGEDATA4<-MERGEDATA3%>%select (contains("Mag"),contains("Subject"),contains("Activity"))
  MERGEDATA5<-MERGEDATA4%>%select (contains("mean"),contains("Subject"),contains("Activity"))
    
  ###QUESTION 3:  Uses descriptive activity names to name the activities in the data set
  Activity<-1:6
  Activity_Description<-c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
  ACTIVITY<-as.data.frame(cbind(Activity,Activity_Description))
  MERGEDATA6<-join(MERGEDATA5,ACTIVITY)
  
  
  
  
  ###QUESTION 4:  Appropriately labels the data set with descriptive variable names. 
  names(MERGEDATA6)<-gsub("fBodyBodyAcc","frequencyBodyAcceleration",names(MERGEDATA6))
  names(MERGEDATA6)<-gsub("fBodyBodyGyro","frequencyBodyAngularVelocity",names(MERGEDATA6))
  names(MERGEDATA6)<-gsub("Mag","Magnitude",names(MERGEDATA6))
  
  
  
  ###QUESTION 5: From the data set in step 4, creates a second, independent tidy data set. 
  ##with the average of each variable for each activity (WALKING...) and each subject (1-2-3) .                        
  
  #Step a: Data will be "melted" to get a narrow set of data.  "Subject" and "Activity_Description" are the ID variables. 
  MERGEDATA7<-melt(MERGEDATA6,id=c("Subject","Activity_Description"))

   #Stepb:  Casting data frames.
  MERGEDATA8<-MERGEDATA7%>%group_by(Subject,Activity_Description,variable)%>%summarise_each(funs(mean),value,na.rm=TRUE)
  
``##Please upload the tidy data set created in step 5 of the instructions. Please upload your data set as a txt file created with write.table() using row.name=FALSE
  write.table(MERGEDATA8,file="TidyDataSet.txt", row.name=FALSE)
