
#SVM
library(e1071)
library(tools)


featuresDir = "/Users/gidutz/Downloads/kaggle/drivers/03_features"

#list all files under Directory
driversFiles=list.files(path =featuresDir,full.names = T )


readDrives <- function(driverLog){
  driverTable = read.csv(file = driverLog)
  driverTable
}

aggregated_distance_matrix = c()

#for each driver
for(driverFile in sample(driversFiles,20)){
  
  # The driver's file is the positive samples
  positive_samples = readDrives(driverFile)  
  
  #take 20 random drives from 5 random drivers
  negavie_sapmles = numeric()
  for(randomDriver in sample(driversFiles[driversFiles!=driverFile],5)){
    alldrives=readDrives(randomDriver)
    negavie_sapmles = rbind(negavie_sapmles,alldrives[sample(200,40),])
  }
  
  #add target values
  positive_samples=cbind(positive_samples,1)
  colnames(positive_samples)[length(positive_samples[1,])]<-"target"
  negavie_sapmles = cbind(negavie_sapmles,0)
  colnames(negavie_sapmles)[length(negavie_sapmles[1,])]<-"target"
  
  #add positive and negative examples to the data set
  data =as.data.frame(rbind(negavie_sapmles,positive_samples))
  
    #shuffel the data
    nr<-dim(data)[1]
    data = data[sample.int(nr),]
  
  #replace all NaNs with colMeans
  f=function(x){
    x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
    x[is.na(x)] =mean(x, na.rm=TRUE) #convert the item with NA to mean value from the column
    x[(x==Inf)] =0 #convert the item with NA to mean value from the column
    x #display the column
  }
  data=apply(data,2,f)  
  
  #split the data into training and testing sets
  train = data[1:(2*nrow(data)/3),]
  test = data[-(1:(2*nrow(data)/3)),]
  
  #Learn using SVM (remove col 1 which indicates the ride index)
  model  <- svm(x= train[,-c(1,2,ncol(train))], y=train[,ncol(train)],kernel ="radial", type="C-classification",cost =5 , degree=30, scale = T)
  
  
  prediction <- predict(model, test[,-c(1,2,ncol(train))])
  validation = cbind(prediction, test[,ncol(test)])
  aggregated_distance_matrix = rbind(aggregated_distance_matrix, validation)
  
  driver = file_path_sans_ext(basename(driverFile))
  
  cat( driver,"\n")
  positive_samples=apply(positive_samples,2,f)  
  
  output = numeric()
  output <-predict(model , positive_samples[,-c(1,2,ncol(positive_samples))])
  outputFile = "/Users/gidutz/Downloads/kaggle/drivers/result/file15.txt"
#   write(paste(driver,"_",positive_samples[,2],",", as.numeric(output)-1,sep = ''), file = outputFile,  append = T )
  
}
confusion_matrix = (table(prediction=aggregated_distance_matrix[,1]-1,actual=aggregated_distance_matrix[,2]))

NPV = confusion_matrix[1,1]/(confusion_matrix[1,1] + confusion_matrix[2,1])
recall = confusion_matrix[2,2]/(confusion_matrix[2,2] + confusion_matrix[1,2])
prescision =  confusion_matrix[2,2]/(confusion_matrix[2,2] + confusion_matrix[2,1])
print(confusion_matrix)
cat(" NPV =\t ",NPV,"\n","recall =\t ",recall,"\n","prescision =\t ",prescision,"\n")




