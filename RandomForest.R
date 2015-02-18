
#Random Forest
library(randomForest)


featuresDir = "/Users/gidutz/Downloads/kaggle/drivers/preprocessing"

#list all files under Directory
driversFiles=list.files(path =featuresDir,full.names = T )

length(driversFiles)<-length(driversFiles)-1

readDrives <- function(driverLog){
  driverTable = read.csv(file = driverLog)
  driverTable
}

predictions = c()

#for each driver
for(driverFile in driversFiles[1]){
  
  # The driver's file is the positive samples
  positive_samples = readDrives(driverFile)  
  
  #take 10 random drives from 10 random drivers
  negavie_sapmles = numeric()
  for(randomDriver in sample(driversFiles[driversFiles!=driverFile],10)){
    alldrives=readDrives(randomDriver)
    negavie_sapmles = rbind(negavie_sapmles,alldrives[sample(200,10),])
  }
  
  #add target values
  positive_samples=cbind(positive_samples,1)
  colnames(positive_samples)[length(positive_samples[1,])]<-"target"
  negavie_sapmles = cbind(negavie_sapmles,0)
  colnames(negavie_sapmles)[length(negavie_sapmles[1,])]<-"target"
  
  train =as.data.frame(rbind(negavie_sapmles,positive_samples))
  #   nr<-dim(train)[1]
  #   train = train[sample.int(nr),]
  
  #replace all NaNs with colMeans
  f=function(x){
    x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
    x[is.na(x)] =mean(x, na.rm=TRUE) #convert the item with NA to mean value from the column
    x[(x==Inf)] =0 #convert the item with NA to mean value from the column
    x #display the column
  }
  
  
  train=apply(train,2,f)  
  
  driver = file_path_sans_ext(basename(driverFile))
  
  cat( driver,"\n")
  
  
  fit <- randomForest( x= train[,-c(1,length(train[1,]))], 
                       y=as.factor(train[,17]),importance = T ,
                       controls=cforest_unbiased(ntree=5, mtry=sample(10)))
  fit = grow(fit, 5)
  varImpPlot(fit)
  positive_samples = train[which(train[,length(train[1,])]==1, arr.ind=T),] #after NA taken cared of
  test = train[,-c(1,length(train[1,]))]
  Prediction <- predict(fit, as.data.frame(test))
}











