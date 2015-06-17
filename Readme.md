# GettingData_Project

**You should create one R script called run_analysis.R that does the following.** 

    1. Merges the training and the test sets to create one data set.
    *We'll need to reassemble the test and the train data sets before we combine them*


```r
##In the furture we may want to add a check that varifies that the zip file is there.
##However for now this is isn't need, and we won't know if the file name might change.

mainfile <- "getdata_projectfiles_UCI HAR Dataset.zip"

X_test <- read.table(unz(mainfile, "UCI HAR Dataset/test/X_test.txt"))
y_test <- read.table(unz(mainfile, "UCI HAR Dataset/test/y_test.txt"))
subject_test <- read.table(unz(mainfile, "UCI HAR Dataset/test/subject_test.txt"))
    ##using the dim function we can see that they are all the same length at 2947 rows
    ##The readme tells me that y is repersents the labels(activity)
    ##subject is an identifier by subject

features <- read.table(unz(mainfile, "UCI HAR Dataset/features.txt"))
activity_labels <- read.table(unz(mainfile, "UCI HAR Dataset/activity_labels.txt"))
    ##This will be used later.

names(X_test) <- features$V2 ##$V2 is needed because v1 is a col 1:561 
names(subject_test) <- "subject"
names(y_test) <- "activity_code"
##Naming is easier to do at this point than after they are combined

test <- cbind(subject_test, y_test, X_test)


##repeat the same process for train

X_train <- read.table(unz(mainfile, "UCI HAR Dataset/train/X_train.txt"))
y_train <- read.table(unz(mainfile, "UCI HAR Dataset/train/y_train.txt"))
subject_train <- read.table(unz(mainfile, "UCI HAR Dataset/train/subject_train.txt"))

names(X_train) <- features$V2 ##name using the same fatures document 
names(subject_train) <- "subject"
names(y_train) <- "activity_code"

train <- cbind(subject_train, y_train, X_train)

##After veiwing the two data sets with head and dim functions.
##We can see that they have the same number of columns with the same names.
##They are ready to be combined.

data1 <- rbind(train, test)
```

    2. Extracts only the measurements on the mean and standard deviation for each measurement. 


```r
##review what columns you wish to keep.
## I chose to hard code the comluns, there may be a way to automate finding the them.
## I tried using the is.element function among others, but was unable to make it work.
## Just writing out what columns after viewing the data will work fine though.

col_list<- c(1:8,43:48,83:88,123:128,163:168,203,204,216,217,229,230,242,243,255,256,268:273,296:298,347:352,375:377,426:431,454:456,505,506,515,518,519,528,531,532,544,545,554,558:563)

data2 <- data1[ , col_list]

##leaves 86, I left any column that had mean or std mentioned in it, including meanfreq.
```

    3. Uses descriptive activity names to name the activities in the data set
    *This step could be done later or in many diffrent ways.  I'm going to be adding a column that has the activity name from activity_lables.txt for the activity code of that row.

```r
for(i in 1:nrow(data2)){
    data2[i,87] <- activity_labels[activity_labels[,1] == data2[i,2],2]
}
## This for loop gives the value of activity_labels column 2 (activity name) where  
## data2 column 2 (activity codes) equals The activity_labels first column 
## ( which is also activity codes)

mean_sd_data <- data2[, c(1,2,87,3:86)] ##I reordered the new column to be 3rd.

colnames(mean_sd_data)[3] <- "activity_names"

##to see the data use - head(data3[,1:6]
```

    4. Appropriately labels the data set with descriptive variable names. 



```r
original_names <- names(mean_sd_data) ##I stored the origina names for the readme

## renaming the data set could be done like this
#names(mean_sd_data)[4:6] <- c("t_acc_x_mean","t_acc_y_mean", "t_acc_z_mean")
#names(mean_sd_data)[7:9] <- c("t_acc_x_sd","t_acc_y_sd", "t_acc_z_sd")
##done in pieces to keep the process more orginized

## However I think the names pulled from the features.txt we appended earlier
## are discriptive and can be used for what we want.
```


    5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    
    *I'm taking this to mean that we want a table where each row is a subject and are now 6 columns for each of the previous varriables.* 


```r
data4 <- aggregate(mean_sd_data[,4:87], by = list(mean_sd_data[,1],mean_sd_data[,3]), FUN = "mean")
##using the aggregate function choose only the varibles you want (column 4 on ward)
##then choose to subset by subject and then activity, applying the mean function.

masterdata <- split(data4, data4$Group.2)
##to see groups - names(masterdata)
## we can now identify groups of each activity by , such as masterdata$LAYING
##Also because we applied the subject column they are already in subject order 1:30

##Now we'll need to rename the columns so that the final data has unqiue names
currentnames <- colnames(masterdata$LAYING) ##all 6 should have the same names

names(masterdata$LAYING) <- paste("Laying", currentnames, sep = "_")
names(masterdata$SITTING) <- paste("Sitting", currentnames, sep = "_")
names(masterdata$STANDING) <- paste("Standing", currentnames, sep = "_")
names(masterdata$WALKING) <- paste("Walking", currentnames, sep = "_")
names(masterdata$WALKING_DOWNSTAIRS) <- paste("WalkDown", currentnames, sep = "_")
names(masterdata$WALKING_UPSTAIRS) <- paste("WalkUp", currentnames, sep = "_")

##Lastly before we combind we need to remove the columns that will be duplicates.
##We could do it after, but in my opinion it's easier now.

LAYING <- masterdata$LAYING[,c(1, 3:86)]##for this first one we keep the subject column
SITTING <- masterdata$SITTING[,3:86]
STANDING <- masterdata$STANDING[,3:86]
WALKING <- masterdata$WALKING[,3:86]
WALK_DOWN <- masterdata$WALKING_DOWNSTAIRS[,3:86]
WALK_UP <- masterdata$WALKING_UPSTAIRS[,3:86]

## and rename the two coulmns of the one we didn't delete

names(LAYING)[1] <- "subject"

final_data <- cbind(LAYING, SITTING, STANDING, WALKING, WALK_DOWN, WALK_UP)

write.table(final_data, file = "tidydata.txt", row.name=FALSE)
```


