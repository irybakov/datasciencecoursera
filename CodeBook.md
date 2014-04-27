CodeBook: Getting and Cleaning Data Project 
========================================================

This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages.
This document describes then algorithms to get *tidy* data set from provided analytic data.

Analytic data is stored in folder *./data/UCI HAR Dataset*

## Source files
Source files to import:

```r
source("./run_analysis.R")
```

## Download Source data
Source analytical data files are *NOT* included in git repository.
If *<Your working directory>/data* does not exist, script will download and upzip source data.  

```r
downloadData()
```

```r
downloadData <- function(){
  # if 'data' directory does not exist, create it
  if (!file.exists("data")) {
    dir.create("data")
  }
  
  # if dataset.zip file does not exist, download it
  if (!file.exists("./data/dataset.zip")) {
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, destfile = "./data/dataset.zip", method = "curl") 
  }
  
  if (!file.exists("./data/UCI HAR Dataset")) {
    unzip("./data/dataset.zip",exdir = "./data/")
  }
}
```

## Init 

```r
# init data root path
path <- "./data/UCI HAR Dataset"

# read feature list
features <- read.table(paste(path, "/features.txt", sep = ""))
print(summary(features))
```

```
##        V1                                 V2     
##  Min.   :  1   fBodyAcc-bandsEnergy()-1,16 :  3  
##  1st Qu.:141   fBodyAcc-bandsEnergy()-1,24 :  3  
##  Median :281   fBodyAcc-bandsEnergy()-1,8  :  3  
##  Mean   :281   fBodyAcc-bandsEnergy()-17,24:  3  
##  3rd Qu.:421   fBodyAcc-bandsEnergy()-17,32:  3  
##  Max.   :561   fBodyAcc-bandsEnergy()-25,32:  3  
##                (Other)                     :543
```

As we can see, There is 561 features total.


```r
# get only -std or -mean headers
headers <- features[grep(".-std|.-mean", features$V2), ]
print(head(headers$V2), 10)
```

```
## [1] "tBodyAcc-mean()-X" "tBodyAcc-mean()-Y" "tBodyAcc-mean()-Z"
## [4] "tBodyAcc-std()-X"  "tBodyAcc-std()-Y"  "tBodyAcc-std()-Z" 
## 477 Levels: "angle(X,gravityMean)" ... "tGravityAccMag-std()"
```

```r
print(length(headers$V2))
```

```
## [1] 79
```

Afer filtering with RegExp we will get 79 features to process.

## Prepare filter before data load
Now we have to prepare vector (cc) to filter columns when reading X data from file. Length of the vertor equals number of featurs.
But only headers that have to be loaded marked 'numeric', other marked 'NULL'.

```r
# vector to filter columns when reading X data
cc <- c(1:length(features$V2))
cc[] <- "NULL"
cc[headers$V1] <- "numeric"
```


Read activity data 

```r
# read activity labels
activity <- read.table(paste(path, "/activity_labels.txt", sep = ""))
print(summary(activity))
```

```
##        V1                        V2   
##  Min.   :1.00   LAYING            :1  
##  1st Qu.:2.25   SITTING           :1  
##  Median :3.50   STANDING          :1  
##  Mean   :3.50   WALKING           :1  
##  3rd Qu.:4.75   WALKING_DOWNSTAIRS:1  
##  Max.   :6.00   WALKING_UPSTAIRS  :1
```


## Training and Test set load and processing
Call prepareTidyData function to process each type of data (**train**, **test**) 


```r
# preparing train tidy data
train <- prepareTidyData(path, "train", activity, cc)
# preparing test tidy data
test <- prepareTidyData(path, "test", activity, cc)
```


Hear is prepareTidyData function body:
```r
prepareTidyData <- function(path,type,activity,cc) {
  data_path <- paste(path,'/',type,sep="")
  x1 <- read.table(paste(data_path,'/X_',type,'.txt',sep=""),colClasses=cc)
  y <- read.table(paste(data_path,'/Y_',type,'.txt',sep=""))
  s <- read.table(paste(data_path,'/subject_',type,'.txt',sep=""))
  # add Activity ID and Subject columns to data
  data <- cbind(y,s,x1)
  afact <- factor(data[,1])
  bfact <- mapvalues(afact, from = as.character(activity$V1), to = as.character(activity$V2))
  data <- cbind(bfact,s,x1)
  data
}
```
Merging tidy results for **train** data with **test** data

```r
# merge train data with test data
tidy <- rbind(train, test)
```


Now we have tidy dataset. 
Preparing headers including *'Activty'* and *'Subject'* 

```r
# get header names
header_names <- as.vector(headers$V2)
# add 'Activty','Subject' headers to header_names
tidy_headers = c("Activty", "Subject", header_names)
colnames(tidy) <- tidy_headers
```


Save tidy data into the file. 

```r
# save tidy data as csv file file location is ./data/tidy_data.csv
write.csv(tidy, "./data/tidy_data.csv", row.names = FALSE)
```


## Summary
Here is tidy data summary:

```r
summary(tidy)
```

```
##                Activty        Subject     tBodyAcc-mean()-X
##  WALKING           :1722   Min.   : 1.0   Min.   :-1.000   
##  WALKING_UPSTAIRS  :1544   1st Qu.: 9.0   1st Qu.: 0.263   
##  WALKING_DOWNSTAIRS:1406   Median :17.0   Median : 0.277   
##  SITTING           :1777   Mean   :16.1   Mean   : 0.274   
##  STANDING          :1906   3rd Qu.:24.0   3rd Qu.: 0.288   
##  LAYING            :1944   Max.   :30.0   Max.   : 1.000   
##  tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y 
##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.000   Min.   :-1.0000  
##  1st Qu.:-0.0249   1st Qu.:-0.1210   1st Qu.:-0.992   1st Qu.:-0.9770  
##  Median :-0.0172   Median :-0.1086   Median :-0.943   Median :-0.8350  
##  Mean   :-0.0177   Mean   :-0.1089   Mean   :-0.608   Mean   :-0.5102  
##  3rd Qu.:-0.0106   3rd Qu.:-0.0976   3rd Qu.:-0.250   3rd Qu.:-0.0573  
##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.000   Max.   : 1.0000  
##  tBodyAcc-std()-Z tGravityAcc-mean()-X tGravityAcc-mean()-Y
##  Min.   :-1.000   Min.   :-1.000       Min.   :-1.000      
##  1st Qu.:-0.979   1st Qu.: 0.812       1st Qu.:-0.243      
##  Median :-0.851   Median : 0.922       Median :-0.144      
##  Mean   :-0.613   Mean   : 0.669       Mean   : 0.004      
##  3rd Qu.:-0.279   3rd Qu.: 0.955       3rd Qu.: 0.119      
##  Max.   : 1.000   Max.   : 1.000       Max.   : 1.000      
##  tGravityAcc-mean()-Z tGravityAcc-std()-X tGravityAcc-std()-Y
##  Min.   :-1.0000      Min.   :-1.000      Min.   :-1.000     
##  1st Qu.:-0.1167      1st Qu.:-0.995      1st Qu.:-0.991     
##  Median : 0.0368      Median :-0.982      Median :-0.976     
##  Mean   : 0.0922      Mean   :-0.965      Mean   :-0.954     
##  3rd Qu.: 0.2162      3rd Qu.:-0.962      3rd Qu.:-0.946     
##  Max.   : 1.0000      Max.   : 1.000      Max.   : 1.000     
##  tGravityAcc-std()-Z tBodyAccJerk-mean()-X tBodyAccJerk-mean()-Y
##  Min.   :-1.000      Min.   :-1.0000       Min.   :-1.0000      
##  1st Qu.:-0.987      1st Qu.: 0.0630       1st Qu.:-0.0186      
##  Median :-0.967      Median : 0.0760       Median : 0.0108      
##  Mean   :-0.939      Mean   : 0.0789       Mean   : 0.0079      
##  3rd Qu.:-0.930      3rd Qu.: 0.0913       3rd Qu.: 0.0335      
##  Max.   : 1.000      Max.   : 1.0000       Max.   : 1.0000      
##  tBodyAccJerk-mean()-Z tBodyAccJerk-std()-X tBodyAccJerk-std()-Y
##  Min.   :-1.0000       Min.   :-1.000       Min.   :-1.000      
##  1st Qu.:-0.0316       1st Qu.:-0.991       1st Qu.:-0.985      
##  Median :-0.0012       Median :-0.951       Median :-0.925      
##  Mean   :-0.0047       Mean   :-0.640       Mean   :-0.608      
##  3rd Qu.: 0.0246       3rd Qu.:-0.291       3rd Qu.:-0.222      
##  Max.   : 1.0000       Max.   : 1.000       Max.   : 1.000      
##  tBodyAccJerk-std()-Z tBodyGyro-mean()-X tBodyGyro-mean()-Y
##  Min.   :-1.000       Min.   :-1.0000    Min.   :-1.0000   
##  1st Qu.:-0.989       1st Qu.:-0.0458    1st Qu.:-0.1040   
##  Median :-0.954       Median :-0.0278    Median :-0.0748   
##  Mean   :-0.763       Mean   :-0.0310    Mean   :-0.0747   
##  3rd Qu.:-0.548       3rd Qu.:-0.0106    3rd Qu.:-0.0511   
##  Max.   : 1.000       Max.   : 1.0000    Max.   : 1.0000   
##  tBodyGyro-mean()-Z tBodyGyro-std()-X tBodyGyro-std()-Y tBodyGyro-std()-Z
##  Min.   :-1.0000    Min.   :-1.000    Min.   :-1.000    Min.   :-1.000   
##  1st Qu.: 0.0648    1st Qu.:-0.987    1st Qu.:-0.982    1st Qu.:-0.985   
##  Median : 0.0863    Median :-0.902    Median :-0.911    Median :-0.882   
##  Mean   : 0.0884    Mean   :-0.721    Mean   :-0.683    Mean   :-0.654   
##  3rd Qu.: 0.1104    3rd Qu.:-0.482    3rd Qu.:-0.446    3rd Qu.:-0.338   
##  Max.   : 1.0000    Max.   : 1.000    Max.   : 1.000    Max.   : 1.000   
##  tBodyGyroJerk-mean()-X tBodyGyroJerk-mean()-Y tBodyGyroJerk-mean()-Z
##  Min.   :-1.0000        Min.   :-1.0000        Min.   :-1.0000       
##  1st Qu.:-0.1172        1st Qu.:-0.0587        1st Qu.:-0.0794       
##  Median :-0.0982        Median :-0.0406        Median :-0.0546       
##  Mean   :-0.0967        Mean   :-0.0423        Mean   :-0.0548       
##  3rd Qu.:-0.0793        3rd Qu.:-0.0252        3rd Qu.:-0.0317       
##  Max.   : 1.0000        Max.   : 1.0000        Max.   : 1.0000       
##  tBodyGyroJerk-std()-X tBodyGyroJerk-std()-Y tBodyGyroJerk-std()-Z
##  Min.   :-1.000        Min.   :-1.000        Min.   :-1.000       
##  1st Qu.:-0.991        1st Qu.:-0.992        1st Qu.:-0.993       
##  Median :-0.935        Median :-0.955        Median :-0.950       
##  Mean   :-0.731        Mean   :-0.786        Mean   :-0.740       
##  3rd Qu.:-0.486        3rd Qu.:-0.627        3rd Qu.:-0.510       
##  Max.   : 1.000        Max.   : 1.000        Max.   : 1.000       
##  tBodyAccMag-mean() tBodyAccMag-std() tGravityAccMag-mean()
##  Min.   :-1.000     Min.   :-1.000    Min.   :-1.000       
##  1st Qu.:-0.982     1st Qu.:-0.982    1st Qu.:-0.982       
##  Median :-0.875     Median :-0.844    Median :-0.875       
##  Mean   :-0.548     Mean   :-0.591    Mean   :-0.548       
##  3rd Qu.:-0.120     3rd Qu.:-0.242    3rd Qu.:-0.120       
##  Max.   : 1.000     Max.   : 1.000    Max.   : 1.000       
##  tGravityAccMag-std() tBodyAccJerkMag-mean() tBodyAccJerkMag-std()
##  Min.   :-1.000       Min.   :-1.000         Min.   :-1.000       
##  1st Qu.:-0.982       1st Qu.:-0.990         1st Qu.:-0.991       
##  Median :-0.844       Median :-0.948         Median :-0.929       
##  Mean   :-0.591       Mean   :-0.649         Mean   :-0.628       
##  3rd Qu.:-0.242       3rd Qu.:-0.296         3rd Qu.:-0.273       
##  Max.   : 1.000       Max.   : 1.000         Max.   : 1.000       
##  tBodyGyroMag-mean() tBodyGyroMag-std() tBodyGyroJerkMag-mean()
##  Min.   :-1.000      Min.   :-1.000     Min.   :-1.000         
##  1st Qu.:-0.978      1st Qu.:-0.978     1st Qu.:-0.992         
##  Median :-0.822      Median :-0.826     Median :-0.956         
##  Mean   :-0.605      Mean   :-0.662     Mean   :-0.762         
##  3rd Qu.:-0.245      3rd Qu.:-0.394     3rd Qu.:-0.550         
##  Max.   : 1.000      Max.   : 1.000     Max.   : 1.000         
##  tBodyGyroJerkMag-std() fBodyAcc-mean()-X fBodyAcc-mean()-Y
##  Min.   :-1.000         Min.   :-1.000    Min.   :-1.000   
##  1st Qu.:-0.992         1st Qu.:-0.991    1st Qu.:-0.979   
##  Median :-0.940         Median :-0.946    Median :-0.864   
##  Mean   :-0.778         Mean   :-0.623    Mean   :-0.537   
##  3rd Qu.:-0.609         3rd Qu.:-0.265    3rd Qu.:-0.103   
##  Max.   : 1.000         Max.   : 1.000    Max.   : 1.000   
##  fBodyAcc-mean()-Z fBodyAcc-std()-X fBodyAcc-std()-Y  fBodyAcc-std()-Z
##  Min.   :-1.000    Min.   :-1.000   Min.   :-1.0000   Min.   :-1.000  
##  1st Qu.:-0.983    1st Qu.:-0.993   1st Qu.:-0.9769   1st Qu.:-0.978  
##  Median :-0.895    Median :-0.942   Median :-0.8326   Median :-0.840  
##  Mean   :-0.665    Mean   :-0.603   Mean   :-0.5284   Mean   :-0.618  
##  3rd Qu.:-0.366    3rd Qu.:-0.249   3rd Qu.:-0.0922   3rd Qu.:-0.302  
##  Max.   : 1.000    Max.   : 1.000   Max.   : 1.0000   Max.   : 1.000  
##  fBodyAcc-meanFreq()-X fBodyAcc-meanFreq()-Y fBodyAcc-meanFreq()-Z
##  Min.   :-1.0000       Min.   :-1.0000       Min.   :-1.0000      
##  1st Qu.:-0.4188       1st Qu.:-0.1448       1st Qu.:-0.1384      
##  Median :-0.2383       Median : 0.0047       Median : 0.0608      
##  Mean   :-0.2215       Mean   : 0.0154       Mean   : 0.0473      
##  3rd Qu.:-0.0204       3rd Qu.: 0.1766       3rd Qu.: 0.2492      
##  Max.   : 1.0000       Max.   : 1.0000       Max.   : 1.0000      
##  fBodyAccJerk-mean()-X fBodyAccJerk-mean()-Y fBodyAccJerk-mean()-Z
##  Min.   :-1.000        Min.   :-1.000        Min.   :-1.000       
##  1st Qu.:-0.991        1st Qu.:-0.985        1st Qu.:-0.987       
##  Median :-0.952        Median :-0.926        Median :-0.948       
##  Mean   :-0.657        Mean   :-0.629        Mean   :-0.744       
##  3rd Qu.:-0.327        3rd Qu.:-0.264        3rd Qu.:-0.513       
##  Max.   : 1.000        Max.   : 1.000        Max.   : 1.000       
##  fBodyAccJerk-std()-X fBodyAccJerk-std()-Y fBodyAccJerk-std()-Z
##  Min.   :-1.000       Min.   :-1.000       Min.   :-1.000      
##  1st Qu.:-0.992       1st Qu.:-0.987       1st Qu.:-0.990      
##  Median :-0.956       Median :-0.928       Median :-0.959      
##  Mean   :-0.655       Mean   :-0.612       Mean   :-0.781      
##  3rd Qu.:-0.320       3rd Qu.:-0.236       3rd Qu.:-0.590      
##  Max.   : 1.000       Max.   : 1.000       Max.   : 1.000      
##  fBodyAccJerk-meanFreq()-X fBodyAccJerk-meanFreq()-Y
##  Min.   :-1.0000           Min.   :-1.0000          
##  1st Qu.:-0.2977           1st Qu.:-0.4280          
##  Median :-0.0454           Median :-0.2365          
##  Mean   :-0.0477           Mean   :-0.2134          
##  3rd Qu.: 0.2045           3rd Qu.: 0.0087          
##  Max.   : 1.0000           Max.   : 1.0000          
##  fBodyAccJerk-meanFreq()-Z fBodyGyro-mean()-X fBodyGyro-mean()-Y
##  Min.   :-1.0000           Min.   :-1.000     Min.   :-1.000    
##  1st Qu.:-0.3314           1st Qu.:-0.985     1st Qu.:-0.985    
##  Median :-0.1025           Median :-0.892     Median :-0.920    
##  Mean   :-0.1238           Mean   :-0.672     Mean   :-0.706    
##  3rd Qu.: 0.0912           3rd Qu.:-0.384     3rd Qu.:-0.473    
##  Max.   : 1.0000           Max.   : 1.000     Max.   : 1.000    
##  fBodyGyro-mean()-Z fBodyGyro-std()-X fBodyGyro-std()-Y fBodyGyro-std()-Z
##  Min.   :-1.000     Min.   :-1.000    Min.   :-1.000    Min.   :-1.000   
##  1st Qu.:-0.985     1st Qu.:-0.988    1st Qu.:-0.981    1st Qu.:-0.986   
##  Median :-0.888     Median :-0.905    Median :-0.906    Median :-0.891   
##  Mean   :-0.644     Mean   :-0.739    Mean   :-0.674    Mean   :-0.690   
##  3rd Qu.:-0.323     3rd Qu.:-0.522    3rd Qu.:-0.439    3rd Qu.:-0.417   
##  Max.   : 1.000     Max.   : 1.000    Max.   : 1.000    Max.   : 1.000   
##  fBodyGyro-meanFreq()-X fBodyGyro-meanFreq()-Y fBodyGyro-meanFreq()-Z
##  Min.   :-1.0000        Min.   :-1.0000        Min.   :-1.0000       
##  1st Qu.:-0.2719        1st Qu.:-0.3626        1st Qu.:-0.2324       
##  Median :-0.0987        Median :-0.1730        Median :-0.0537       
##  Mean   :-0.1010        Mean   :-0.1743        Mean   :-0.0514       
##  3rd Qu.: 0.0681        3rd Qu.: 0.0137        3rd Qu.: 0.1225       
##  Max.   : 1.0000        Max.   : 1.0000        Max.   : 1.0000       
##  fBodyAccMag-mean() fBodyAccMag-std() fBodyAccMag-meanFreq()
##  Min.   :-1.000     Min.   :-1.000    Min.   :-1.0000       
##  1st Qu.:-0.985     1st Qu.:-0.983    1st Qu.:-0.0966       
##  Median :-0.875     Median :-0.855    Median : 0.0703       
##  Mean   :-0.586     Mean   :-0.659    Mean   : 0.0769       
##  3rd Qu.:-0.217     3rd Qu.:-0.382    3rd Qu.: 0.2450       
##  Max.   : 1.000     Max.   : 1.000    Max.   : 1.0000       
##  fBodyBodyAccJerkMag-mean() fBodyBodyAccJerkMag-std()
##  Min.   :-1.000             Min.   :-1.000           
##  1st Qu.:-0.990             1st Qu.:-0.991           
##  Median :-0.929             Median :-0.925           
##  Mean   :-0.621             Mean   :-0.640           
##  3rd Qu.:-0.260             3rd Qu.:-0.308           
##  Max.   : 1.000             Max.   : 1.000           
##  fBodyBodyAccJerkMag-meanFreq() fBodyBodyGyroMag-mean()
##  Min.   :-1.000                 Min.   :-1.000         
##  1st Qu.:-0.003                 1st Qu.:-0.983         
##  Median : 0.164                 Median :-0.876         
##  Mean   : 0.173                 Mean   :-0.697         
##  3rd Qu.: 0.357                 3rd Qu.:-0.451         
##  Max.   : 1.000                 Max.   : 1.000         
##  fBodyBodyGyroMag-std() fBodyBodyGyroMag-meanFreq()
##  Min.   :-1.000         Min.   :-1.0000            
##  1st Qu.:-0.978         1st Qu.:-0.2344            
##  Median :-0.828         Median :-0.0521            
##  Mean   :-0.700         Mean   :-0.0416            
##  3rd Qu.:-0.471         3rd Qu.: 0.1516            
##  Max.   : 1.000         Max.   : 1.0000            
##  fBodyBodyGyroJerkMag-mean() fBodyBodyGyroJerkMag-std()
##  Min.   :-1.000              Min.   :-1.000            
##  1st Qu.:-0.992              1st Qu.:-0.993            
##  Median :-0.945              Median :-0.938            
##  Mean   :-0.780              Mean   :-0.792            
##  3rd Qu.:-0.612              3rd Qu.:-0.644            
##  Max.   : 1.000              Max.   : 1.000            
##  fBodyBodyGyroJerkMag-meanFreq()
##  Min.   :-1.0000                
##  1st Qu.:-0.0195                
##  Median : 0.1362                
##  Mean   : 0.1267                
##  3rd Qu.: 0.2890                
##  Max.   : 1.0000
```


## Second, independent tidy data set with the average of each variable for each activity and each subject
Second data set has 180 rows

```r
# melt tidy data
tMelt <- melt(tidy, id = c("Activty", "Subject"))
# dcast using mean function
avgData <- dcast(tMelt, Subject + Activty ~ variable, mean)
write.csv(avgData, "./data/tidy_mean_data.csv", row.names = FALSE)

nrow(avgData)
```

```
## [1] 180
```

```r
summary(avgData)
```

```
##     Subject                   Activty   tBodyAcc-mean()-X
##  Min.   : 1.0   WALKING           :30   Min.   :0.222    
##  1st Qu.: 8.0   WALKING_UPSTAIRS  :30   1st Qu.:0.271    
##  Median :15.5   WALKING_DOWNSTAIRS:30   Median :0.277    
##  Mean   :15.5   SITTING           :30   Mean   :0.274    
##  3rd Qu.:23.0   STANDING          :30   3rd Qu.:0.280    
##  Max.   :30.0   LAYING            :30   Max.   :0.301    
##  tBodyAcc-mean()-Y  tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y 
##  Min.   :-0.04051   Min.   :-0.1525   Min.   :-0.996   Min.   :-0.9902  
##  1st Qu.:-0.02002   1st Qu.:-0.1121   1st Qu.:-0.980   1st Qu.:-0.9421  
##  Median :-0.01726   Median :-0.1082   Median :-0.753   Median :-0.5090  
##  Mean   :-0.01788   Mean   :-0.1092   Mean   :-0.558   Mean   :-0.4605  
##  3rd Qu.:-0.01494   3rd Qu.:-0.1044   3rd Qu.:-0.198   3rd Qu.:-0.0308  
##  Max.   :-0.00131   Max.   :-0.0754   Max.   : 0.627   Max.   : 0.6169  
##  tBodyAcc-std()-Z tGravityAcc-mean()-X tGravityAcc-mean()-Y
##  Min.   :-0.988   Min.   :-0.680       Min.   :-0.4799     
##  1st Qu.:-0.950   1st Qu.: 0.838       1st Qu.:-0.2332     
##  Median :-0.652   Median : 0.921       Median :-0.1278     
##  Mean   :-0.576   Mean   : 0.698       Mean   :-0.0162     
##  3rd Qu.:-0.231   3rd Qu.: 0.943       3rd Qu.: 0.0877     
##  Max.   : 0.609   Max.   : 0.975       Max.   : 0.9566     
##  tGravityAcc-mean()-Z tGravityAcc-std()-X tGravityAcc-std()-Y
##  Min.   :-0.4951      Min.   :-0.997      Min.   :-0.994     
##  1st Qu.:-0.1173      1st Qu.:-0.983      1st Qu.:-0.971     
##  Median : 0.0238      Median :-0.970      Median :-0.959     
##  Mean   : 0.0741      Mean   :-0.964      Mean   :-0.952     
##  3rd Qu.: 0.1495      3rd Qu.:-0.951      3rd Qu.:-0.937     
##  Max.   : 0.9579      Max.   :-0.830      Max.   :-0.644     
##  tGravityAcc-std()-Z tBodyAccJerk-mean()-X tBodyAccJerk-mean()-Y
##  Min.   :-0.991      Min.   :0.0427        Min.   :-0.03869     
##  1st Qu.:-0.961      1st Qu.:0.0740        1st Qu.: 0.00047     
##  Median :-0.945      Median :0.0764        Median : 0.00947     
##  Mean   :-0.936      Mean   :0.0795        Mean   : 0.00757     
##  3rd Qu.:-0.918      3rd Qu.:0.0833        3rd Qu.: 0.01340     
##  Max.   :-0.610      Max.   :0.1302        Max.   : 0.05682     
##  tBodyAccJerk-mean()-Z tBodyAccJerk-std()-X tBodyAccJerk-std()-Y
##  Min.   :-0.06746      Min.   :-0.995       Min.   :-0.990      
##  1st Qu.:-0.01060      1st Qu.:-0.983       1st Qu.:-0.972      
##  Median :-0.00386      Median :-0.810       Median :-0.776      
##  Mean   :-0.00495      Mean   :-0.595       Mean   :-0.565      
##  3rd Qu.: 0.00196      3rd Qu.:-0.223       3rd Qu.:-0.148      
##  Max.   : 0.03805      Max.   : 0.544       Max.   : 0.355      
##  tBodyAccJerk-std()-Z tBodyGyro-mean()-X tBodyGyro-mean()-Y
##  Min.   :-0.993       Min.   :-0.2058    Min.   :-0.2042   
##  1st Qu.:-0.983       1st Qu.:-0.0471    1st Qu.:-0.0896   
##  Median :-0.884       Median :-0.0287    Median :-0.0732   
##  Mean   :-0.736       Mean   :-0.0324    Mean   :-0.0743   
##  3rd Qu.:-0.512       3rd Qu.:-0.0168    3rd Qu.:-0.0611   
##  Max.   : 0.031       Max.   : 0.1927    Max.   : 0.0275   
##  tBodyGyro-mean()-Z tBodyGyro-std()-X tBodyGyro-std()-Y tBodyGyro-std()-Z
##  Min.   :-0.0725    Min.   :-0.994    Min.   :-0.994    Min.   :-0.986   
##  1st Qu.: 0.0747    1st Qu.:-0.974    1st Qu.:-0.963    1st Qu.:-0.961   
##  Median : 0.0851    Median :-0.789    Median :-0.802    Median :-0.801   
##  Mean   : 0.0874    Mean   :-0.692    Mean   :-0.653    Mean   :-0.616   
##  3rd Qu.: 0.1018    3rd Qu.:-0.441    3rd Qu.:-0.420    3rd Qu.:-0.311   
##  Max.   : 0.1791    Max.   : 0.268    Max.   : 0.476    Max.   : 0.565   
##  tBodyGyroJerk-mean()-X tBodyGyroJerk-mean()-Y tBodyGyroJerk-mean()-Z
##  Min.   :-0.1572        Min.   :-0.0768        Min.   :-0.09250      
##  1st Qu.:-0.1032        1st Qu.:-0.0455        1st Qu.:-0.06172      
##  Median :-0.0987        Median :-0.0411        Median :-0.05343      
##  Mean   :-0.0961        Mean   :-0.0427        Mean   :-0.05480      
##  3rd Qu.:-0.0911        3rd Qu.:-0.0384        3rd Qu.:-0.04898      
##  Max.   :-0.0221        Max.   :-0.0132        Max.   :-0.00694      
##  tBodyGyroJerk-std()-X tBodyGyroJerk-std()-Y tBodyGyroJerk-std()-Z
##  Min.   :-0.997        Min.   :-0.997        Min.   :-0.995       
##  1st Qu.:-0.980        1st Qu.:-0.983        1st Qu.:-0.985       
##  Median :-0.840        Median :-0.894        Median :-0.861       
##  Mean   :-0.704        Mean   :-0.764        Mean   :-0.710       
##  3rd Qu.:-0.463        3rd Qu.:-0.586        3rd Qu.:-0.474       
##  Max.   : 0.179        Max.   : 0.296        Max.   : 0.193       
##  tBodyAccMag-mean() tBodyAccMag-std() tGravityAccMag-mean()
##  Min.   :-0.9865    Min.   :-0.987    Min.   :-0.9865      
##  1st Qu.:-0.9573    1st Qu.:-0.943    1st Qu.:-0.9573      
##  Median :-0.4829    Median :-0.607    Median :-0.4829      
##  Mean   :-0.4973    Mean   :-0.544    Mean   :-0.4973      
##  3rd Qu.:-0.0919    3rd Qu.:-0.209    3rd Qu.:-0.0919      
##  Max.   : 0.6446    Max.   : 0.428    Max.   : 0.6446      
##  tGravityAccMag-std() tBodyAccJerkMag-mean() tBodyAccJerkMag-std()
##  Min.   :-0.987       Min.   :-0.993         Min.   :-0.995       
##  1st Qu.:-0.943       1st Qu.:-0.981         1st Qu.:-0.977       
##  Median :-0.607       Median :-0.817         Median :-0.801       
##  Mean   :-0.544       Mean   :-0.608         Mean   :-0.584       
##  3rd Qu.:-0.209       3rd Qu.:-0.246         3rd Qu.:-0.217       
##  Max.   : 0.428       Max.   : 0.434         Max.   : 0.451       
##  tBodyGyroMag-mean() tBodyGyroMag-std() tBodyGyroJerkMag-mean()
##  Min.   :-0.981      Min.   :-0.981     Min.   :-0.9973        
##  1st Qu.:-0.946      1st Qu.:-0.948     1st Qu.:-0.9852        
##  Median :-0.655      Median :-0.742     Median :-0.8648        
##  Mean   :-0.565      Mean   :-0.630     Mean   :-0.7364        
##  3rd Qu.:-0.216      3rd Qu.:-0.360     3rd Qu.:-0.5119        
##  Max.   : 0.418      Max.   : 0.300     Max.   : 0.0876        
##  tBodyGyroJerkMag-std() fBodyAcc-mean()-X fBodyAcc-mean()-Y
##  Min.   :-0.998         Min.   :-0.995    Min.   :-0.9890  
##  1st Qu.:-0.981         1st Qu.:-0.979    1st Qu.:-0.9536  
##  Median :-0.881         Median :-0.769    Median :-0.5950  
##  Mean   :-0.755         Mean   :-0.576    Mean   :-0.4887  
##  3rd Qu.:-0.577         3rd Qu.:-0.217    3rd Qu.:-0.0634  
##  Max.   : 0.250         Max.   : 0.537    Max.   : 0.5242  
##  fBodyAcc-mean()-Z fBodyAcc-std()-X fBodyAcc-std()-Y  fBodyAcc-std()-Z
##  Min.   :-0.990    Min.   :-0.997   Min.   :-0.9907   Min.   :-0.987  
##  1st Qu.:-0.962    1st Qu.:-0.982   1st Qu.:-0.9404   1st Qu.:-0.946  
##  Median :-0.724    Median :-0.747   Median :-0.5134   Median :-0.644  
##  Mean   :-0.630    Mean   :-0.552   Mean   :-0.4815   Mean   :-0.582  
##  3rd Qu.:-0.318    3rd Qu.:-0.197   3rd Qu.:-0.0791   3rd Qu.:-0.266  
##  Max.   : 0.281    Max.   : 0.658   Max.   : 0.5602   Max.   : 0.687  
##  fBodyAcc-meanFreq()-X fBodyAcc-meanFreq()-Y fBodyAcc-meanFreq()-Z
##  Min.   :-0.636        Min.   :-0.3795       Min.   :-0.5201      
##  1st Qu.:-0.392        1st Qu.:-0.0813       1st Qu.:-0.0363      
##  Median :-0.257        Median : 0.0079       Median : 0.0658      
##  Mean   :-0.232        Mean   : 0.0115       Mean   : 0.0437      
##  3rd Qu.:-0.061        3rd Qu.: 0.0863       3rd Qu.: 0.1754      
##  Max.   : 0.159        Max.   : 0.4665       Max.   : 0.4025      
##  fBodyAccJerk-mean()-X fBodyAccJerk-mean()-Y fBodyAccJerk-mean()-Z
##  Min.   :-0.995        Min.   :-0.989        Min.   :-0.992       
##  1st Qu.:-0.983        1st Qu.:-0.973        1st Qu.:-0.980       
##  Median :-0.813        Median :-0.782        Median :-0.871       
##  Mean   :-0.614        Mean   :-0.588        Mean   :-0.714       
##  3rd Qu.:-0.282        3rd Qu.:-0.196        3rd Qu.:-0.470       
##  Max.   : 0.474        Max.   : 0.277        Max.   : 0.158       
##  fBodyAccJerk-std()-X fBodyAccJerk-std()-Y fBodyAccJerk-std()-Z
##  Min.   :-0.995       Min.   :-0.991       Min.   :-0.9931     
##  1st Qu.:-0.985       1st Qu.:-0.974       1st Qu.:-0.9837     
##  Median :-0.825       Median :-0.785       Median :-0.8951     
##  Mean   :-0.612       Mean   :-0.571       Mean   :-0.7565     
##  3rd Qu.:-0.247       3rd Qu.:-0.169       3rd Qu.:-0.5438     
##  Max.   : 0.477       Max.   : 0.350       Max.   :-0.0062     
##  fBodyAccJerk-meanFreq()-X fBodyAccJerk-meanFreq()-Y
##  Min.   :-0.5760           Min.   :-0.6020          
##  1st Qu.:-0.2897           1st Qu.:-0.3975          
##  Median :-0.0609           Median :-0.2321          
##  Mean   :-0.0691           Mean   :-0.2281          
##  3rd Qu.: 0.1766           3rd Qu.:-0.0472          
##  Max.   : 0.3314           Max.   : 0.1957          
##  fBodyAccJerk-meanFreq()-Z fBodyGyro-mean()-X fBodyGyro-mean()-Y
##  Min.   :-0.6276           Min.   :-0.993     Min.   :-0.994    
##  1st Qu.:-0.3087           1st Qu.:-0.970     1st Qu.:-0.970    
##  Median :-0.0919           Median :-0.730     Median :-0.814    
##  Mean   :-0.1376           Mean   :-0.637     Mean   :-0.677    
##  3rd Qu.: 0.0386           3rd Qu.:-0.339     3rd Qu.:-0.446    
##  Max.   : 0.2301           Max.   : 0.475     Max.   : 0.329    
##  fBodyGyro-mean()-Z fBodyGyro-std()-X fBodyGyro-std()-Y fBodyGyro-std()-Z
##  Min.   :-0.986     Min.   :-0.995    Min.   :-0.994    Min.   :-0.987   
##  1st Qu.:-0.962     1st Qu.:-0.975    1st Qu.:-0.960    1st Qu.:-0.964   
##  Median :-0.791     Median :-0.809    Median :-0.796    Median :-0.822   
##  Mean   :-0.604     Mean   :-0.711    Mean   :-0.645    Mean   :-0.658   
##  3rd Qu.:-0.264     3rd Qu.:-0.481    3rd Qu.:-0.415    3rd Qu.:-0.392   
##  Max.   : 0.492     Max.   : 0.197    Max.   : 0.646    Max.   : 0.522   
##  fBodyGyro-meanFreq()-X fBodyGyro-meanFreq()-Y fBodyGyro-meanFreq()-Z
##  Min.   :-0.3958        Min.   :-0.6668        Min.   :-0.5075       
##  1st Qu.:-0.2134        1st Qu.:-0.2943        1st Qu.:-0.1548       
##  Median :-0.1155        Median :-0.1579        Median :-0.0508       
##  Mean   :-0.1046        Mean   :-0.1674        Mean   :-0.0572       
##  3rd Qu.: 0.0027        3rd Qu.:-0.0427        3rd Qu.: 0.0415       
##  Max.   : 0.2492        Max.   : 0.2731        Max.   : 0.3771       
##  fBodyAccMag-mean() fBodyAccMag-std() fBodyAccMag-meanFreq()
##  Min.   :-0.987     Min.   :-0.988    Min.   :-0.3123       
##  1st Qu.:-0.956     1st Qu.:-0.945    1st Qu.:-0.0147       
##  Median :-0.670     Median :-0.651    Median : 0.0813       
##  Mean   :-0.536     Mean   :-0.621    Mean   : 0.0761       
##  3rd Qu.:-0.162     3rd Qu.:-0.365    3rd Qu.: 0.1744       
##  Max.   : 0.587     Max.   : 0.179    Max.   : 0.4358       
##  fBodyBodyAccJerkMag-mean() fBodyBodyAccJerkMag-std()
##  Min.   :-0.994             Min.   :-0.994           
##  1st Qu.:-0.977             1st Qu.:-0.975           
##  Median :-0.794             Median :-0.813           
##  Mean   :-0.576             Mean   :-0.599           
##  3rd Qu.:-0.187             3rd Qu.:-0.267           
##  Max.   : 0.538             Max.   : 0.316           
##  fBodyBodyAccJerkMag-meanFreq() fBodyBodyGyroMag-mean()
##  Min.   :-0.1252                Min.   :-0.987         
##  1st Qu.: 0.0453                1st Qu.:-0.962         
##  Median : 0.1720                Median :-0.766         
##  Mean   : 0.1625                Mean   :-0.667         
##  3rd Qu.: 0.2759                3rd Qu.:-0.409         
##  Max.   : 0.4881                Max.   : 0.204         
##  fBodyBodyGyroMag-std() fBodyBodyGyroMag-meanFreq()
##  Min.   :-0.982         Min.   :-0.4566            
##  1st Qu.:-0.949         1st Qu.:-0.1695            
##  Median :-0.773         Median :-0.0535            
##  Mean   :-0.672         Mean   :-0.0360            
##  3rd Qu.:-0.428         3rd Qu.: 0.0823            
##  Max.   : 0.237         Max.   : 0.4095            
##  fBodyBodyGyroJerkMag-mean() fBodyBodyGyroJerkMag-std()
##  Min.   :-0.998              Min.   :-0.998            
##  1st Qu.:-0.981              1st Qu.:-0.980            
##  Median :-0.878              Median :-0.894            
##  Mean   :-0.756              Mean   :-0.771            
##  3rd Qu.:-0.583              3rd Qu.:-0.608            
##  Max.   : 0.147              Max.   : 0.288            
##  fBodyBodyGyroJerkMag-meanFreq()
##  Min.   :-0.1829                
##  1st Qu.: 0.0542                
##  Median : 0.1116                
##  Mean   : 0.1259                
##  3rd Qu.: 0.2081                
##  Max.   : 0.4263
```


