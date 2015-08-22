
setwd("C:/Users/Vijay/Desktop/Coursera_R_Progamming/Getting_Cleaning_data/course_Project")
library(httr)
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
des_file<-"Dataset.zip"
download.file(url,des_file)

datafolder<- "UCI HAR Dataset"
resultfolder<- "result"

#unzip and create folders (if those ain't exist)

if(!file.exists(des_file))
        {
        print("unzip file")
        unzip("Dataset.zip",list = FALSE,overwrite = TRUE)
        }

if(!file.exists(resultfolder)){
        print("create result folder")
        dir.create(resultfolder)
}

#read txt and covnert to data.frame

gettables<- function(filename,cols=NULL){
        print(paste("Getting table:",filename))
        f<- paste(datafolder,filename,sep="/")
        data<-data.frame()
        if(is.null(cols)){
                data<- read.table(f,sep="",stringsAsFactors = F)
                
        }else{
                data<- read.table(f,sep = "",stringsAsFactors = F,col.names=cols)
        }
    data
}

#run and check gettables
features<- gettables("features.txt")

#read data and build database
getdata<- function(type,features){
        print(paste("Getting Data",type))
        subject_data<-gettables(paste(type,"/","subject_",type,".txt",sep = ""),"id")
        y_data<-gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
        x_data<- gettables(paste(type,"/","X_",type,".txt",sep = ""),features$V2)
        return(cbind(subject_data,y_data,x_data))
}

#run and check getdata
test<- getdata("test",features)
train<- getdata("train",features)

#save the resulting data in the indicated folder
saveresult<- function(data,name){
        print(paste("Saving Results", name))
        file<- paste(resultfolder,"/",name,".csv",sep = "")
        write.csv(data,file)
}

### required activities ###


library(plyr)

#1) Merges the training and the test sets to create one data set.

data<- rbind(train,test)
data<- arrange(data,id)

#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_and_std <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveresult(mean_and_std,"mean_and_std")

#3) Uses descriptive activity names to name the activities in the data set
activity_labels <- gettables("activity_labels.txt")

#4) Appropriately labels the data set with descriptive variable names. 
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidy_dataset <- ddply(mean_and_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")
saveresult(tidy_dataset,"tidy_dataset")

