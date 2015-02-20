library("tools")

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

getAngle<-function(u,v){
  x1 = u[1]
  x2 = v[1]
  y1 = u[2]
  y2 = v[2]
  dot = x1*x2 + y1*y2      # dot product
  det = x1*y2 - y1*x2      # determinant
  angle = atan2(det, dot)  # atan2(y, x) or atan2(sin, cos)
  return(angle)
}

#moving average of n steps
smoothFilter <- rep(1/4, 4)


#calculates driver features
featureCalculator <-function(driveLog, DriverFile){
  
  #create features vector for the drive
  features =numeric();
  
  #save the drive number (first feature)
  driveNumber = file_path_sans_ext(basename(DriverFile))
  
  #calculate velocities
  driveVelocity=mps2kmph(distances(driveLog))
  driveVelocity[driveVelocity>200]=120
  
  #calculate acceleration
  driveAcceleration = diff(driveVelocity) 
  
  #0 drive number
  features = c(features,"driveNumber" = driveNumber )
  
  #1 max velocity
  features = c(features,"max"=max(driveVelocity) )
  
  #2 median velocity
  features = c(features,quantile(driveVelocity, c(.5)))
  
  #3 10th percentile velocity
  features = c(features,quantile(driveVelocity, c(.1)))
  
  #4 90th percentile velocity
  features = c(features,quantile(driveVelocity, c(.9)))
  
  #5 90th percentile of accelerations
  features = c(features,"90th percentile of accelerations"=quantile(driveAcceleration, c(.9)))
  
  #6 10th percentile of accelerations
  features = c(features,"10th percentile of accelerations" = quantile(driveAcceleration, c(.1)))
  
  #7 time of ride
  features = c(features,"time of ride" = length(driveVelocity))
  
  #8 length of ride
  features = c(features,"length of ride" = sum(distances(driveLog)))
  
  #9 average velocity
  features = c(features,"average velocity"=mean(driveVelocity))
  
  #10 mean velocity between 20-40 
  features = c(features,"mean velocity between 20-40 " = mean(driveVelocity[(driveVelocity>20)&(driveVelocity<40)]))
  
  #11 mean velocity 20-60
  features = c(features,"mean velocity 20-60" = mean(driveVelocity[(driveVelocity>20)&(driveVelocity<60)]))
  
  #13 mean velocity 40-60
  features = c(features,"mean velocity 40-60"=mean(driveVelocity[(driveVelocity>40)&(driveVelocity<60)]))
  
  #14 mean acceleration <5 (ABS)
  features = c(features,"mean acceleration <5 (ABS)" = mean(driveAcceleration[abs(driveAcceleration)<5]))
  
  #15 mean velocity 60+
  features = c(features,"mean velocity 60+"=mean(driveVelocity[(driveVelocity>60)]))
  
  #16 mean velocity 60-90
  features = c(features,"mean velocity 60-90"=mean(driveVelocity[(driveVelocity>60)&(driveVelocity<90)]))
  
  #17 mean acceleration <2 (ABS)
  features = c(features,"mean acceleration <2 (ABS)" = mean(driveAcceleration[abs(driveAcceleration)<2]))
  
  #18 %time at const velocity (no accelerations)
  features = c(features,"%time at const velocity" = length(driveAcceleration[abs(driveAcceleration)<0.3])/length(driveAcceleration))
  
  #19 SD velocity between 20-40 
  features = c(features,"SD velocity between 20-40 " = sd(driveVelocity[(driveVelocity>20)&(driveVelocity<40)],na.rm = T))
  
  #20 SD velocity between 20-60
  features = c(features,"SD velocity between 20-60 " = sd(driveVelocity[(driveVelocity>20)&(driveVelocity<60)],na.rm = T))
  
  #21 SD velocity between 20-60
  features = c(features,"SD velocity between 40-70 " = sd(driveVelocity[(driveVelocity>40)&(driveVelocity<70)],na.rm = T))
  
  #22 Ratio between acceleration (-2)/2
  low_slowdowns = driveAcceleration[(driveAcceleration>(-2))&(driveAcceleration<0.05)]
  low_starts = driveAcceleration[(driveAcceleration>0.05)&(driveAcceleration<2)]
  features = c(features,"Ratio between acceleration (-2)/2" = mean(low_slowdowns)/mean(low_starts))


features
}

#load all driver dirs
directories =list.dirs(path = "/Users/gidutz/Downloads/drivers",recursive=F);
for(directory in directories){
  #lists all the drive under driver folder
  filesList = list.files(path = directory, pattern = "[[:digit:]]+.csv", full.names = T)
  
  #this will be the feature table
  features_table = numeric()
  
  for(DriverFile in filesList){
    driveLog =read.csv(DriverFile, header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE)
    features_table = rbind(features_table,t(featureCalculator(driveLog,DriverFile)))
  }
  
  outpath = "/Users/gidutz/Downloads/kaggle/drivers/03_features/"
  
  write.csv(features_table,file=paste(outpath,basename(directory),".csv",sep=''))
  cat(basename(directory),"\n")
  
}


