# run_analysis.R
# This script is used to extract and clean mean and standard deviation data 
# from the train and test data of UCI_HAR data. 

# citation: Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. A Public Domain Dataset for Human Activity Recognition Using Smartphones. 21th European Symposium on Artificial Neural Networks, Computational Intelligence and Machine Learning, ESANN 2013. Bruges, Belgium 24-26 April 2013.

# Finding variable of interest -----------------------------------------
# Goal: the X-files contain 561 variables per measurement, which is listed in
# the features file. The goal of this part is to find out the # of variables (col's) 
# that has mean or std dev of each measurement. 
features <- read.table("./data/UCI_HAR/features.txt")
meanvar <- grep("mean\\()", features$V2) # V# of the mean values
meanfea <- grep("mean\\()", features$V2, value=TRUE) # Feature name of the mean values
sdvar <- grep("std\\()", features$V2) # V# of the tsd values
sdfea <- grep("std\\()", features$V2, value=TRUE) # Feature name of the std values
vars <- c(meanvar, sdvar) # Col#'s that will be used for data extraction
varnames <- c(meanfea, sdfea) # Will be assigned as col names


# Data loading ---------------------------------------------------------
xtrain <- read.table("./data/UCI_HAR/train/X_train.txt")
# dim(xtrain): 7352 561
ytrain <- read.table("./data/UCI_HAR/train/y_train.txt") # activity
# dim(ytrain): 7352 1
strain <- read.table("./data/UCI_HAR/train/subject_train.txt") # 21 subjects
# dim(strain): 7352 1

xtest <- read.table("./data/UCI_HAR/test/X_test.txt")
# dim(xtrain): 2947 561
ytest <- read.table("./data/UCI_HAR/test/y_test.txt") # activity
# dim(ytrain): 2947 1
stest <- read.table("./data/UCI_HAR/test/subject_test.txt") # 9 subjects
# dim(strain): 2947 1


# Data filtering ---------------------------------------------------------
# Select only Mean and Std variables from X files
xtrainsum <- xtrain[,vars]
xtestsum <- xtest[,vars]
# Note: for now variables with means are grouped together, 
# then variables with std's.


# Variable name reformatting and variable renaming ------------------------
# Clean varnames (removing "()" but keep "-")
varnames2 <- sub("\\()", "", varnames,) 
colnames(xtrainsum)
# Substitute colume names in datasets with varnames2:
colnames(xtrainsum) <- varnames2
colnames(xtestsum) <- varnames2


# Data combining and renaming ----------------------------------------------
library(dplyr) # or plyr?
# combine test, add type=test, etc:
traindata <- cbind (strain, ytrain, xtrainsum)
traindata <- mutate(traindata, type="train")
traindata <- dplyr::rename(traindata, subject = V1, activity = V1.1) # renaming the unamed variables
# combine train, add type=train, etc:
testdata <- cbind (stest, ytest, xtestsum)
testdata <- mutate(testdata, type="test")
testdata <- dplyr::rename(testdata, subject = V1, activity = V1.1) 
# combine test and train data
fulldata <- rbind(traindata, testdata)
# Note: it might be considered better to use 0 and 1 to represent test and 
# train in the "type" variable.


# Changing activity from number code to descriptive name ---------------------
fulldata2 <- mutate(fulldata, activity = case_when(
        activity == 1 ~ "walking",
        activity == 2 ~ "walkingUp",
        activity == 3 ~ "walkingDown",
        activity == 4 ~ "sitting",
        activity == 5 ~ "standing",
        activity == 6 ~ "laying"
))
# to double check, compare table(fulldata$activity) to table(fulldata2$activity)


# Save dataset to file ----------------------------------------------------
write.csv(fulldata2, file = "./data/UCIHARclean.csv", row.names=FALSE)

# =========================================================================
# Part 2: creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.
# Note: whether data come from train or test does not seem to matter, 
# so "type" variable will be removed. 

# Calculate mean per activity and subject --------------------------------
summarymean <- select(fulldata2, -(type)) %>% 
        dplyr::group_by(activity, subject) %>% 
        dplyr::summarise_all(mean)


# Save dataset to file ----------------------------------------------------
write.csv(summarymean, file = "./data/UCIHARmean.csv", row.names=FALSE)

