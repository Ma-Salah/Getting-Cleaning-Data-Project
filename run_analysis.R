library(dplyr)
testx <- read.table("UCI HAR Dataset/test/X_test.txt")
testy <- read.table("UCI HAR Dataset/test/Y_test.txt")
subtest <- read.table("UCI HAR Dataset/test/subject_test.txt")
trainx <- read.table("UCI HAR Dataset/train/X_train.txt")
trainy <- read.table("UCI HAR Dataset/train/Y_train.txt")
subtrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")
#step 1 : the training and the test data set
actv <- testy$V1
subj <- subtest$V1
testdata <- mutate(testx, actv, subj)
actv <- trainy$V1
subj <- subtrain$V1
traindata <- mutate(trainx,actv, subj)
dataset <- bind_rows(testdata,traindata)
dataset <- tbl_df(dataset)
View(dataset)
#--------------------------------------------------------------------#
#--------------------------------------------------------------------#
# step 2: measurements on the mean and standard deviation
featvars <- features$V2
meanstd <- select(dataset,grep("mean()",featvars),grep("std()",featvars))
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
# step 3 : descriptive activity names
actvitynames <- as.factor(dataset$actv)
a <- activitylabels$V2
a <-tolower(activitylabels$V2)
a <- gsub("_","",a)
levels(actvitynames) <- a
dataset <- mutate(dataset,actv=actvitynames)
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
# step 4 ; descriptive variable names
featvars <- as.character(featvars)
dup <- duplicated(featvars)
dupno <- which(dup)
featvars[dupno][1:14] <- gsub("-","x",featvars[dupno][1:14])
featvars[dupno][15:28] <- gsub("-","y",featvars[dupno][15:28])
featvars[dupno][29:42] <- gsub("-","x",featvars[dupno][29:42])
featvars[dupno][43:56] <- gsub("-","y",featvars[dupno][43:56])
featvars[dupno][57:70] <- gsub("-","x",featvars[dupno][57:70])
featvars[dupno][71:84] <- gsub("-","y",featvars[dupno][71:84])
featvars <- gsub("-","",featvars)
featvars <- gsub("\\()","",featvars)
featvars <- gsub(",","",featvars)
names(dataset)[1:561] <- featvars
#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
# step 5 : average of each variable for each activity and each subject.
dataset <- group_by(dataset,actv,subj)
newdataset <- summarise_all(dataset,mean)
write.table(newdataset,file="step5.txt",  row.name=FALSE)
