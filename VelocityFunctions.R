#moving average of n steps
smoothFilter <- rep(1/4, 4)



#Creates plots showing the drive's velocity
velocity_viewer <-function(driveLog, DriverFile){
  driveLog_back_sec= rbind( c(0,0),driveLog)  
  driveLog = rbind(driveLog,c(0,0))
  
  driveVelocity=(sqrt((driveLog[,1]-driveLog_back_sec[,1])**2+
                        (driveLog[,2]-driveLog_back_sec[,2])**2))
  length(driveVelocity)<-length(driveVelocity)-10
  
  #converts M/s to kM/h
  driveVelocity = 3.6*driveVelocity
  
  plot(driveVelocity,type="l",xlab = "time",main = DriverFile)
  driveVelocity_back_sec = append( driveVelocity,0, after=0)
  
  driveAcceleration = driveVelocity - driveVelocity_back_sec
  
  #Smooth Using mooving average
  driveAcceleration = filter(driveAcceleration,smoothFilter,sides=1)
  plot(driveAcceleration,type="l",xlab = "time",main = DriverFile)
  hist(driveAcceleration)
  
}