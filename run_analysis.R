library("plyr")
library("reshape2")

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

prepareTidyData <- function(path,type,activity,cc) {
  data_path <- paste(path,'/',type,sep="")
  x1 <- read.table(paste(data_path,'/X_',type,'.txt',sep=""),colClasses=cc)
  y <- read.table(paste(data_path,'/Y_',type,'.txt',sep=""))
  s <- read.table(paste(data_path,'/subject_',type,'.txt',sep=""))
  # add Activity ID and Subject columns to train data
  data <- cbind(y,s,x1)
  afact <- factor(data[,1])
  bfact <- mapvalues(afact, from = as.character(activity$V1), to = as.character(activity$V2))
  data <- cbind(bfact,s,x1)
  data
}

run <- function(){
  path <- './data/UCI HAR Dataset'
  
  downloadData()
  
  # read feature list
  features <- read.table(paste(path,'/features.txt',sep=""))
  # get only -std or -mean headers
  headers <- features[grep(".-std|.-mean",features$V2),]
  # get header names
  header_names <- as.vector(headers$V2)
  
  # vector to filter columns when reading X data 
  cc <- c(1:length(features$V2))
  cc[] <- 'NULL'
  cc[headers$V1] <- 'numeric'
  
  # read activity labels
  activity <- read.table(paste(path,'/activity_labels.txt',sep=""))
  
  # preparing train tidy data
  train <- prepareTidyData(path,'train',activity,cc)

  # preparing test tidy data 
  test <- prepareTidyData(path,'test',activity,cc)

  # add 'Activty','Subject' headers to header_names
  tidy_headers = c('Activty','Subject',header_names)
  
  # merge train data with test data
  tidy <- rbind(train,test) 
  colnames(tidy) <- tidy_headers
  # save tidy data as csv file
  write.csv(tidy,'./data/tidy_data.csv',row.names=FALSE)
  print('Tidy Data saved in ./data/tidy_data.csv file')
  
  # melt tidy data
  tMelt <- melt(tidy,id=c("Activty","Subject"))
  # dcast using mean function
  avgData <- dcast(tMelt, Subject + Activty  ~ variable,mean)
  write.csv(avgData,'./data/tidy_mean_data.csv',row.names=FALSE)
  print('Tidy Mean Data saved in ./data/tidy_mean_data.csv file')
  
}



