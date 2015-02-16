library("MASS")
library("mvtnorm")
library("parallel")
library("nnet")
library("ggplot2")

readDrives=function(path){
  alldrives=list()
  files=list.files(path,full.names=T)
  #   for(i in 1:length(files)){
  #     alldrives[[i]]=read.csv(files[i])
  #   }
  alldrives=lapply(1:length(files),function(x){read.csv(files[x])})
  return(alldrives)
}
mps2kmph=function(speed){  return((speed/1000)*3600)}
discrete_derivative=function(data){
  (data[-1]-data[-length(data)])
}
gradient=function(data){
  apply(data,2,discrete_derivative)
}
distances= function(adrive){
  grad=gradient(adrive)**2
  dists=sqrt(grad[,1]+grad[,2])
}

predictions=c()
setwd("D:/Machine Learning/kaggle telmatics/drivers/drivers/")
driversFolder=list.files("D:/Machine Learning/kaggle telmatics/drivers/drivers/")

getFeatures=function(trip){
    feature_row=c()
    #total distance traveled
    total_distance=sum(distances(trip))
    #adding aerial distance
    aerial_dist=sqrt(sum(trip[c(1,nrow(trip)),]**2))
    #adding length of time
    travel_time=nrow(trip)
    #adding max speed
    speeds=mps2kmph(distances(trip))
    speeds[speeds>200]=90
    top_speed=max(speeds)
  return(c(total_distance,aerial_dist,travel_time,top_speed))
}

random_features=c()
for(randomDriver in sample(driversFolder,5)){
  alldrives=readDrives(randomDriver)
  driver_features=cbind(t(sapply(alldrives,getFeatures)),0)
  random_features=rbind(random_features,driver_features)
}
predictions=c()
system.time(for(driver in driversFolder){
  alldrives=readDrives(driver)
  driver_features=cbind(t(sapply(alldrives,getFeatures)),1)
  write.csv(driver_features,paste("D:/Machine Learning/kaggle telmatics/features/",driver,".csv",sep=""))
#   train=rbind(random_features,driver_features)
#   model=glm(V5~.,as.data.frame(train), family = binomial("logit"))
#   curr_predictions=predict(model,as.data.frame(driver_features[,-5]),type="response")
#   
#   curr_pred_line =cbind(driver_trip=paste(driver,"_",1:length(alldrives),sep=""),prob=curr_predictions)
#   predictions=rbind(predictions,curr_pred_line)
#   if(as.integer(driver)%%10 ==0){print(driver)}
})
write.csv(predictions,"D:/Machine Learning/kaggle telmatics/My Submissions/firstSupervised.csv",row.names=F,quote=F)
