library("MASS")
library("mvtnorm")
library("parallel")
library("nnet")
library("ggplot2")

readDrives=function(path){
  alldrives=list()
  files=list.files(path,,full.names=T)
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
setwd("/Users/gidutz/Downloads/drivers/")
driversFolder=list.files("/Users/gidutz/Downloads/drivers/")

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
    
    
    speeds_back_sec = append( speeds,0, after=0)
    
    accelerations = append( speeds,0, after=length(speeds)) - append( speeds,0, after=0)
    
    
    
    #   1. avg speed when velocity > 1 kmh
    avg = mean(subset(speeds,speeds>1)) 
    if(!is.finite(avg)){avg = 50}
    
    #   2. avg speed 0-20
    avg0_20 =  mean(subset(speeds,speeds>1&speeds<20))
    if(!is.finite(avg0_20)){avg0_20 = 10}
    
    
    #   3. avg speed 20-40
    avg20_40 =  mean(subset(speeds,speeds>20&speeds<40))    
    if(!is.finite(avg20_40)){avg20_40 = 30}
    
    
    #   4. avg speed 40-65
    avg40_65 =  mean(subset(speeds,speeds>40&speeds<65))
    if(!is.finite(avg40_65)){avg40_65 = 52}
    
    
    #   5. avg speed 60-95
    avg60_95 =  mean(subset(speeds,speeds>60&speeds<95))   
    if(!is.finite(avg60_95)){avg60_95 = 85}
    
    #   6. avg speed 95+
    avg_95 = mean(subset(speeds,speeds>95))    
    if(!is.finite(avg60_95)){avg60_95 = 120}
    
    #   7. sd speed when velocity > 1 kmh
    sd0 = sd(subset(speeds,speeds>1)) 
    if(!is.finite(sd0)){sd0 = 0}
    
    #   8. sd speed 0-20
    sd0_20 =  sd(subset(speeds,speeds>1&speeds<20))
    if(!is.finite(sd0_20)){sd0_20 = 0}
    
    
    #   9. sd speed 20-40
    sd20_40 =  sd(subset(speeds,speeds>20&speeds<40))    
    if(!is.finite(sd20_40)){sd20_40 = 0}
    
    
    #   10. sd speed 40-65
    sd40_65 =  sd(subset(speeds,speeds>40&speeds<65))
    if(!is.finite(sd40_65)){sd40_65 = 0}
    
    
    #   11. sd speed 60-95
    sd60_95 =  sd(subset(speeds,speeds>60&speeds<95))   
    if(!is.finite(sd60_95)){sd60_95 = 5}
    
    #   12. sd speed 95+
    sd_95 = sd(subset(speeds,speeds>95))    
    if(!is.finite(sd60_95)){sd60_95 = 5}
    
    
    #   13. %acceletation (-2:-0.05)/%acceletation (0.05:2)
    accelaration_low = length(subset(accelerations,accelerations>-2& accelerations<(-0.05)))/
      length(subset(accelerations,accelerations>0.05&accelerations<2))
    if(!is.finite(accelaration_low)){accelaration_low = 0}
    
    #   8. %acceletation (-6:-2) / total accelrations (where acceleration >0.05)
    accelaration_strong_stops = length(subset(accelerations,accelerations>-6&accelerations<(-2)))/
      length(accelerations)
    if(!is.finite(accelaration_strong_stops)){accelaration_strong_stops = 0}
    
    #   9. %acceletation (2:6) /total accelrations (where acceleration >0.05)
    accelaration_strong_starts = length(subset(accelerations,accelerations>2&accelerations<6))/
      length(accelerations)
    if(!is.finite(accelaration_strong_starts)){accelaration_strong_starts = 0}
    
    #   10. %acceletation (<-6)/total accelrations (where acceleration >0.05)
    accelaration_stop_crazy = length(subset(accelerations,accelerations<(-6)))/length(accelerations)
    if(!is.finite(accelaration_stop_crazy)){accelaration_stop_crazy = 0}
    
    
    #   11.%acceleration (>6)/total acceletations   (where acceleration >0.05)
    accelaration_start_crazy = length(subset(accelerations,accelerations>6))/length(accelerations)
    if(!is.finite(accelaration_start_crazy)){accelaration_start_crazy = 0}
    
    
    
    
  return(c(total_distance,aerial_dist,travel_time,top_speed, avg,avg0_20,  avg20_40,avg40_65 ,  avg60_95, avg_95 ,sd0 , sd0_20,   sd20_40, sd40_65 , sd60_95 , sd_95 ,accelaration_low ,accelaration_strong_stops , accelaration_strong_starts, accelaration_stop_crazy ,accelaration_start_crazy 
           
           
  ))
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
  write.csv(driver_features,paste("/Users/gidutz/Downloads/kaggle/drivers/features/",driver,".csv",sep=""))
  train=rbind(random_features,driver_features)
  model=glm(V22~.,as.data.frame(train), family = binomial("logit"))
  curr_predictions=predict(model,as.data.frame(driver_features[,-5]),type="response")
  
  curr_pred_line =cbind(driver_trip=paste(driver,"_",1:length(alldrives),sep=""),prob=curr_predictions)
  predictions=rbind(predictions,curr_pred_line)
  if(as.integer(driver)%%10 ==0){print(driver)}
})
write.csv(predictions,"/Users/gidutz/Downloads/kaggle/drivers/submit/firstSupervised.csv",row.names=F,quote=F)
