#####################################################

library("RCurl")
library (bitops)
library (methods)
bhavcopy <- function()
{
  mydata <- read.table("database.csv", header=TRUE,sep=",")
  temp<-dim(mydata)   
  URLs<-temp[2]
  temp1<-mydata[URLs]
  sURL<-temp1[,1]
  sURL<-as.character(sURL)
  success<-sapply(sURL,download)
}

#####################################################


download<-function(sURL)
{
  Sys.sleep(poisson())
  if(!is.na(sURL))
  {
    options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6;en-US; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12")
    tContent <- getURLContent(sURL, verbose = TRUE, useragent = getOption("HTTPUserAgent"))
    attributes(tContent) = NULL
    c<-substr(sURL, 62 , 84)
    writeBin(tContent, c )    
  }
}

#####################################################

poisson<- function()
{
  k = 0
  p = 1.0
  L <-exp(-4)
  while (p >= L)  
  {
    k<-k+1
    x1 <- runif(1, 0, 1)
    p <- p*x1
  }
  k<- k-1
  return (k)
}
#####################################################
