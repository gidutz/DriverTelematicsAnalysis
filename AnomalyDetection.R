directories =list.dirs(path = "/Users/gidutz/Downloads/drivers/", recursive=F);

directories = directories[1:(length(directories)-1)]
directories = "/Users/gidutz/Downloads/drivers/2184/"
prob <- function (x, miu,n, sigma){
  exp(-0.5*t(x-miu)%*%(solve(sigma)%*%(x-miu)))/(((2*pi)**0.5*n)*(det(sigma)**0.5))
}

for(directory in directories){
  fileTalbe = paste(directory,"/ValueSTDandMean.csv",sep='')
  samples = read.csv(file = fileTalbe)
  samples = samples[,c( 1:4,7,12:15)+1]  
  samples = 1000*scale(samples, center=FALSE, scale=colSums(samples))
  miu = colMeans(samples)
  sigma = cov(samples)
  n = length(samples[,1])
  val=apply(samples,1,function(row){prob(row,miu,n,sigma)})
  driver = basename(directory)
  
  cat( driver,"\n")#,quantile(val,c(0.1,0.2,0.3,0.4,0.5,0.6,0.9)),"\n")
  #hist(val)
  epsilon = 5.0e-5
  suspected = which(val<epsilon, arr.ind=T)
  cleared = which(val>=epsilon, arr.ind=T)
  
  outputFile = "/Users/gidutz/Downloads/drivers/result/file5.txt"
  write(paste(driver,"_",suspected,",0" ,sep = ''), file = outputFile,  append = T, )
  write(paste(driver,"_",cleared ,",1" ,sep = ''), file = outputFile, append = T, )
  
}






