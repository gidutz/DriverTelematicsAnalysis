

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


#moving average of n steps
smoothFilter <- rep(1/4, 4)


#Creates plots showing the drive's velocity
velocity_viewer <-function(driveLog, DriverFile){
  
  features =numeric();
  
  
  
  driveVelocity=mps2kmph(distances(driveLog))
  driveVelocity[driveVelocity>200]=90
  driveAcceleration = diff(driveVelocity) 
  
  
  #1 max velocity
  features = c(features,max(driveVelocity) )
  
  #2 median velocity
  features = c(features,quantile(driveVelocity, c(.5)))
  
  #3 10th percentile velocity
  features = c(features,quantile(driveVelocity, c(.1)))
  
  #3 90th percentile velocity
  features = c(features,quantile(driveVelocity, c(.9)))
  
  #4 %time at const velocity (no accelerations)
  features = c(features,length(abs(driveAcceleration)<0.3)/length(driveAcceleration))
  
  #5 90th percentile of accelerations
  features = c(features,quantile(driveVelocity, c(.9)))
  
  #6 10th percentile of accelerations
  features = c(features,quantile(driveVelocity, c(.1)))
  
  #7 time of ride
  features = c(features,length(driveVelocity))
  
  #8 length of ride
  features = c(features,sum(distances(driveLog)))
  
  #9 average velocity
  features = c(features,mean(driveVelocity))
  
  #9 SD velocity
  features = c(features,sd(driveVelocity))
  
  
  features
}

directories =list.dirs(path = "/Users/gidutz/Downloads/drivers",recursive=F);
for(directory in directories){
  filesList = list.files(path = directory, pattern = "[[:digit:]]+.csv", full.names = T)
  results = numeric()
  for(DriverFile in filesList){
    driveLog =read.csv(DriverFile, header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE)
    results = rbind(results,t(velocity_viewer(driveLog,DriverFile)))
  }
  
  
  
  outpath = "/Users/gidutz/Downloads/kaggle/drivers/01_features/"
  
  
  #View(results)
  write.csv(results,file=paste(outpath,basename(directory),".csv",sep=''))
  cat(basename(directory),"\n")
  
}

