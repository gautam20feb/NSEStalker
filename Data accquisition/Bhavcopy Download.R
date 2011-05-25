library("RCurl")
library (bitops)
library (methods)

data <- function()
{
  
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
  down<- function(a)
{
  
  if(!is.na(a))
  {
 Sys.sleep(2) 
    options(HTTPUserAgent = "Mozilla/3.73 [en] (X11 ; U ; Ubuntu 10.04)")
    tContent <- getURLContent(a, verbose = TRUE, useragent = getOption("HTTPUserAgent"))
    attributes(tContent) = NULL
    c<-substr(a, 62 , 84)
    writeBin(tContent, c)
  }
}
mydata <- read.table("database.csv", header=TRUE,
   sep=",")
temp<-dim(mydata)  
URLs<-temp[2]
temp2<-mydata[URLs]
sURL <-temp2[,1]
sURL<-as.character(sURL)
sapply(sURL,down)
}