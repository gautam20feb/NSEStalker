## set the directory as working 
# load(file=".RData")
i = 1   ##<< just to initialise. later comment it as the last i will be restored from .Rdata
j = 1   ##<< just to initialise. later comment it as the last i will be restored from .Rdata

library(timeDate)
library(fBasics)
library(IBrokers)
library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(xts)
tws<-twsConnect(clientId=1)

exp.dt<-get.nearest.expiry.date(start.date="2010-06-15", end.date="2011-07-14")

source("../lib/read_machines.R")
source("../lib/create_connection.R")
connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database

#################################### Initialization ################################################
stk<-read.csv("../data/STK.with.Sym.csv", sep=",", header=T)
 tt<-exp.dt[n,]
 exp.dt<-rbind(exp.dt,tt ,row.names=NULL)
 no<- nrow(stk)
 n<-  nrow(exp.dt)
 

## Function to start the execution 
# Tws.per.min.data.to.db<-function(
user.name="intern@Ophelia"
## The default user name is intern
# )
#   {
  machines=GenerateMachimesDataframe(config_path)
  conn <- CreateConnection(user.name , machines ,"TWS_PERMIN_futures")
 
 exp<- exp.dt[,2]
 
while(i <=no)  ##<< Loop for different stocks
    {
    s<-stk[i,3]
    s<-as.character(s)
    tablename<-as.character(stk[i,4])
  		j<- 1
      q1<-0
      q<-0
      r<-0
      
     while(j <= n )  ##<< Loop for all the different set of all days for any stock
       {
         #### j is the current working day 
         currexp<- exp[j]
         ind.exp<-which(exp==currexp)
         
         ind.exp<-tail(ind.exp,n =1L)
         if(ind.exp==n)
         {
           ind.exp<-ind.exp-1
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
    #   print(date)
     #  Sys.sleep(8)
      currdate<-exp.dt[j-1,1] 
      print(paste("currdate ",as.character(currdate)))
       print(paste("EXp ",as.character(currexp)))
     
      t<-twsFuture(symbol=s,exch="NSE",expiry=expiry.date,currency="INR",include_expired = '1')
      reqHistoricalData(tws,t ,file=paste("../data/downloaded/TWS_PERMIN_futures/",as.character(stk[i,2]),".per_min_fut",exp.dt[j-1,1],".csv") ,bar="1 min", dur="5 D" , endDateTime=enddatetime)
      
#       t<-twsFuture(symbol="ABAN",exch="NSE",expiry="201007",currency="INR",include_expired = '1')
#       reqHistoricalData(tws,t ,file=paste("../data/downloaded/TWS_PERMIN_futures/",as.character(stk[i,2]),".per_min_fut",currdate,".csv") ,bar="1 min", dur="5 D" , endDateTime="20100724 00:00:00")
      
      Sys.sleep(4)
    try(  d <- read.table(file=paste("../data/downloaded/TWS_PERMIN_futures/",as.character(stk[i,2]),".per_min_fut",exp.dt[j-1,1],".csv"),header = F, sep = ",")
     , silent=TRUE)
      if(nrow(d)>0)
      {
      data<-data.frame()
      data<-cbind(d,currexp)
      names(data)<-c("TIMESTAMP", "OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT","EXPIRY_DT")
      if(nrow(data)==0)
      {
        cat(as.character(timestamp()),paste("added to",tablename," ERROR ",enddatetime,sep=" ") , "\n",file = connection.log, sep = ",")
      }
       else
       {
         ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
       cat(as.character(timestamp()),paste("added to",tablename,"data for last 5 days with end date",enddatetime,sep=" ") , "\n",file = connection.log, sep = ",")
       }
         
      }
        options(save.defaults=list(ascii=TRUE,safe=FALSE))
         save.image(file=".RData")
      
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
     
          t<-twsFuture(symbol=s,exch="NSE",expiry=expiry.date,currency="INR",include_expired = '1')
      reqHistoricalData(tws,t ,file=paste("../data/downloaded/TWS_PERMIN_futures/",as.character(stk[i,2]),".per_min_fut",exp.dt[j-1,1],".csv") ,bar="1 min", dur=paste(r," D",sep="") , endDateTime=enddatetime) 
   
          Sys.sleep(4)
     try( d <- read.table(file=paste("../data/downloaded/TWS_PERMIN_futures/",as.character(stk[i,2]),".per_min_fut",exp.dt[j-1,1],".csv"),header = F, sep = ",")
      ,silent=TRUE)
      
      if(nrow(d))
      {
      data<-data.frame()
      data<-cbind(d,currexp)
      names(data)<-c("TIMESTAMP", "OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT","EXPIRY_DT")
      if(nrow(data)==0)
      {
        cat(as.character(timestamp()),paste("added to",tablename," ERROR ",enddatetime,sep=" ") , "\n",file = connection.log, sep = ",")
      }
       else
       {
         ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
       cat(as.character(timestamp()),paste("added to",tablename,"data for last ",r, "days with end date",enddatetime,sep=" ") , "\n",file = connection.log, sep = ",")
       }
         
        
         }
          options(save.defaults=list(ascii=TRUE,safe=FALSE))
save.image(file=".RData")
         
        }
         if(j==n)
         {
           j<-j+10
           options(save.defaults=list(ascii=TRUE,safe=FALSE))
save.image(file=".RData")
           }
      } 
    }
     
             


    
    
     

