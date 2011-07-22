library(timeDate)
library(fBasics)
library(IBrokers)
library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(xts)
library(gregmisc)


UpdatePerminFutures <- function(
### Update the permin stocks
connection = connection.TWS_PERMIN_futures,
tablename,
start.date ="2011-06-02",
version = 0) 
{

  futures_list<-trim(read.csv("../data/futures_list.csv", sep=",", header=T))
  no<- nrow(futures_list)
  exp.dt<-get.nearest.expiry.date(start.date=start.date, end.date=Sys.Date())
  VERSION = version + 1
# #################################### Initialization ################################################
  n<-  nrow(exp.dt)
  tt<-exp.dt[n,]
   exp.dt<-rbind(exp.dt,tt ,row.names=NULL)
   exp<- exp.dt[,2]
  n<-  nrow(exp.dt)
    conn <- connection
    i = which(futures_list[,4]==tablename)
    s<-futures_list[i,3]
    s<-as.character(s)
  		j<- 1
      q1<-0
      q<-0
      r<-0
      
     while( j <= n )  ##<< Loop for all the different set of all days for any stock
       {
         #### j is the current working day 
       d <- data.frame() 
       currexp <- exp[j]
         ind.exp<-which(exp==currexp)
         
         ind.exp<-tail(ind.exp,n =1L)
         if(ind.exp==n)
         {
           ind.exp <- ind.exp-1
         }
         print(paste("Index : ", ind.exp))
         t<-ind.exp-q1
         print(paste("length : ", t))
         q = as.integer(t) %/%5
         print(paste("Q",as.character(q)))
          
         r = t%%5
         print(paste("R",as.character(r)))
         q1<-ind.exp
         while(q > 0)
         {
           Sys.sleep(5)
            print(paste("J",as.character(j)))
       j<-j+5
       
       print(paste("J",as.character(j)))
       q<-q-1
       enddatetimetemp<- exp.dt[j,1]
       temp1<-strsplit(enddatetimetemp, "-")
       temp2<-unlist(temp1)
       enddate<-paste(temp2[1],temp2[2],temp2[3], sep="")
       enddatetime<-paste(enddate, " 00:00:00",sep="")  ##<< Gives the date in required format
      
       temp1<-strsplit(currexp, "-")
       temp2<-unlist(temp1)
       exdate<-paste(temp2[1],temp2[2],temp2[3], sep="")
       
       expiry.date<-substr(exdate,1,6)
      currdate<-exp.dt[j-1,1] 
      print(paste("currdate ",as.character(currdate)))
       print(paste("EXp ",as.character(currexp)))
     file.name=paste("../data/downloaded/update/TWS_PERMIN_futures/",as.character(futures_list[i,2])," .per_min_fut ",exp.dt[j-1,1],".csv",sep="")
#       t<-twsFuture(symbol=s,exch="NSE",expiry=expiry.date,currency="INR",include_expired = '1')
#       reqHistoricalData(tws,t ,file= file.name ,bar="1 min", dur="5 D" , endDateTime=enddatetime)
 
      Sys.sleep(4)
    try(  d <- read.table(file=file.name,header = F, sep = ",")
     , silent=TRUE)
      if(nrow(d)>0)
      {
      data<-data.frame()
      data<-cbind(d,currexp,VERSION)
      names(data)<-c("TIMESTAMP", "OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT","EXPIRY_DT","VERSION")
      if(nrow(data)==0)
      {
      cat("TWS_PERMIN_futures",timestamp(),"UpdatePerminFutures",paste("ERROR IN ",tablename, sep= "") , "\n" , file=connection.log , sep = ",")
      }
       else
       {
         ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
      cat("TWS_PERMIN_futures",timestamp(),"UpdatePerminFutures",paste("Added",tablename,enddatetimetemp, sep= " ") , "\n" , file=connection.log , sep = ",")
       }
         
      }
      
         }
        if(r>0){
          print(paste("J",as.character(j)))
           j<-j+r
           print(paste("J",as.character(j)))
           Sys.sleep(5)
      enddatetimetemp<- exp.dt[j,1]
       temp1<-strsplit(enddatetimetemp, "-")
       temp2<-unlist(temp1)
       enddate<-paste(temp2[1],temp2[2],temp2[3], sep="")
       enddatetime<-paste(enddate, " 00:00:00",sep="")  ##<< Gives the date in required format    
       temp1<-strsplit(currexp, "-")
       temp2<-unlist(temp1)
       exdate<-paste(temp2[1],temp2[2],temp2[3], sep="")
       
       expiry.date<-substr(exdate,1,6)
      currdate<-exp.dt[j-1,1] 
      print(paste("currdate ",as.character(currdate)))
       print(paste("EXp ",as.character(currexp)))

       
#       t<-twsFuture(symbol=s,exch="NSE",expiry=expiry.date,currency="INR",include_expired = '1')
       file.name = paste("../data/downloaded/update/TWS_PERMIN_futures/",as.character(futures_list[i,2])," .per_min_fut ",exp.dt[j-1,1],".csv",sep="")

#       reqHistoricalData(tws,t ,file= file.name ,bar="1 min", dur=paste(r," D",sep="") , endDateTime=enddatetime) 
    Sys.sleep(4)
     try( d <- read.table(file=file.name ,header = F, sep = ",")
      ,silent=TRUE)
      
      if(nrow(d))
      {
      data<-data.frame()
      data<-cbind(d,currexp,VERSION)
      names(data)<-c("TIMESTAMP", "OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT","EXPIRY_DT","VERSION")
      if(nrow(data)==0)
      {
      cat("TWS_PERMIN_futures",timestamp(),"UpdatePerminFutures",paste("ERROR IN ",tablename, sep= "") , "\n" , file=connection.log , sep = ",")
      }
       else
       {
         ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
      cat("TWS_PERMIN_futures",timestamp(),"UpdatePerminFutures",paste("Added",tablename,enddatetimetemp, sep= " ") , "\n" , file=connection.log , sep = ",")
       }
      }
         
        }
         if(j==n)
         {
           j<-j+10
           }
      } 
    }

