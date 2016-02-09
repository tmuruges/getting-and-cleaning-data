#
# Download contents from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# Then, extract the relevant files into a folder to be named "UCIHARDataset" in the working directory


# 1. Read the data 
subtest <- read.table("./UCIHARDataset/test/subject_test.txt")
xtest <- read.table("./UCIHARDataset/test/X_test.txt")
ytest <- read.table("./UCIHARDataset/test/Y_test.txt")
subtrain <- read.table("./UCIHARDataset/train/subject_train.txt")
xtrain <- read.table("./UCIHARDataset/train/X_train.txt")
ytrain <- read.table("./UCIHARDataset/train/Y_train.txt")
features <- read.table("./UCIHARDataset/features.txt")
activities <- read.table("./UCIHARDataset/activity_labels.txt")

# 2. Merge all that data into a single dataset
xall <- rbind(xtrain, xtest)
yall <- rbind(ytrain, ytest)
suball <- rbind(subtrain, subtest)
combined <- cbind(suball, yall, xall)


# 3. Add the feature names to coloumn, and give names to the id columns
featureNames <- as.character(features[,2])
newCols <- c("subject", "activity", featureNames)
colnames(combined) <- newCols

# 4. Create a new data frame contain only mean and st. dev features
onlyMeans <- grep("mean()", colnames(combined))
onlyStDevs <- grep("std()", colnames(combined))
revisedColumns <- c(onlyMeans, onlyStDevs)
updatedColumns <- sort(revisedColumns) 
newDataFrame <- combined[, c(1,2,updatedColumns)]
updatedDataFrame <- newDataFrame[, !grepl("Freq", colnames(newDataFrame))] 


# 5. Minimise the rows to the 180 needed to show mean values for each subject-activity pair
tidyframe <- data.frame()
for (i in 1:30) {
        subj<- subset(updatedDataFrame,subject==i)
        for (j in 1:6){
                actv<- subset(subj, activity==j)
                myresult<-as.vector(apply(actv,2,mean))
                tidyframe<-rbind(tidyframe,myresult) 
        }
        
}

# 6. Rename output data to "tidy_Data.txt"
colnames(tidyframe)<-colnames(updatedDataFrame) 
levels(tidyframe[,2])<-c("walking","walking_upstairs","walking_downstairs", "sitting","standing", "laying")
write.table(tidyframe, "tidy_Data.txt",row.name=FALSE, sep = "\t")

