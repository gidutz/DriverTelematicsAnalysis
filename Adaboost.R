library("rpart")
library("ada")
# Adaboost


#SVM
library(e1071)
library(tools)


featuresDir = "/Users/gidutz/Downloads/kaggle/drivers/01_features"

#list all files under Directory
driversFiles=list.files(path =featuresDir,full.names = T )

#length(driversFiles)<-length(driversFiles)-1

readDrives <- function(driverLog){
  driverTable = read.csv(file = driverLog)
  driverTable
}

predictions = c()
aggregated_distance_matrix = c()

#for each driver
for(driverFile in driversFiles[40:46]){
  
  # The driver's file is the positive samples
  positive_samples = readDrives(driverFile)  
  
  #take 10 random drives from 10 random drivers
  negavie_sapmles = numeric()
  for(randomDriver in sample(driversFiles[driversFiles!=driverFile],50)){
    alldrives=readDrives(randomDriver)
    negavie_sapmles = rbind(negavie_sapmles,alldrives[sample(200,1),])
  }
  
  #add target values
  positive_samples=cbind(positive_samples,1)
  colnames(positive_samples)[length(positive_samples[1,])]<-"target"
  negavie_sapmles = cbind(negavie_sapmles,0)
  colnames(negavie_sapmles)[length(negavie_sapmles[1,])]<-"target"
  
  #add positive and negative examples to the data set
  data =as.data.frame(rbind(negavie_sapmles,positive_samples))
  data = data[-6]
  
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
  
  train = data[1:(nrow(data)/3),]
  test = data[-(1:(nrow(data)/3)),]
  
  model  <- ada(x= train[,-c(1,ncol(train))], y=train[,ncol(train)], ,iter=20,nu=1,type="discrete")
  model=addtest(model,test[,-c(1,ncol(train))],test[,ncol(test)])
  
  varplot(model)
  
  
  prediction <- predict(model, newdata = as.data.frame(test[,-c(1,ncol(train))]), type = c("vector", "probs", "both","F"))
  validation = cbind(prediction, test[,ncol(test)])
  
  
  driver = file_path_sans_ext(basename(driverFile))
  
  cat( driver,"\n")
  
  #   outputFile = "/Users/gidutz/Downloads/kaggle/drivers/result/file9.txt"
  #   write(paste(driver,"_",c(1:length(prediction)),",", prediction,sep = ''), file = outputFile,  append = T )
  
}







