library(IBrokers)
library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(xts)


UpdatePerminStocks <- function(
### Update the permin stocks
connection ,
tablename ,
start.date ,
version ) 
{

conn<- connection
  
############################## Getting the list of working day #########################################
holiday<-read.csv("../data/holiday.csv", header = T)
tS = timeSequence(from = start.date, to = Sys.Date(), by = "day")
char<-as.character(tS)
day<-dayOfWeek(tS)
hols<-as.character(seq(length(char))) ##<<  The holiday or working day  
  
### Marking all the Weekends as holidays
for(i in 1:length(char))    
{
 if(day[i]=="Sat" || day[i]=="Sun")
 {
  hols[i]<-"Holiday"
 }
 else
 {  
  hols[i]<-"Working Day"
  }
### Marking all the holidays as as listed in holiday.csv
for(j in 1:nrow(holiday))     
{
   if(as.character(char[i]) == as.character(holiday[j,1]))
  {
  hols[i]<-"Holiday"
   }
  }
  }
  

wday<-c()  ##<< list of all the working days
### creating working days
for(i in 1:length(tS))
  {
    if(hols[i]=="Working Day")  ##<< Creating URLs for working days
    {
        wday<-c(wday,char[i]) 
    }
    
  }

cat("",timestamp(),"UpdatePerminStocks","Got the list of working days", "\n" , file=connection.log , sep = ",")

#################################### Initialization ################################################
  futures_list<-trim(read.csv("../data/futures_list.csv", sep=",", header=T))
  no<- nrow(futures_list)

  n<-length(wday)
  sequence<-seq(5,n,by=5)
  VERSION = version +1
    i = which(futures_list[,4] == tablename)
    #Sys.sleep(300)
    for(j in sequence)  ##<< Loop for all the different set of 4 days for any stock
      {
       d <- data.frame()
       data <- data.frame()
       enddatetime<- wday[j]
       temp1<-strsplit(enddatetime, "-")
       temp2<-unlist(temp1)
       date<-paste(temp2[1],temp2[2],temp2[3], sep="")
       print(date)
       Sys.sleep(8)
       s<-futures_list[i,3]
       s<-as.character(s)
       edt<-paste(date, " 00:00:00",sep="")  ##<< Gives the date in required format
      file.name = paste("../data/downloaded/TWS_PERMIN_stocks/",as.character(futures_list[i,2])," .per.day.min ",date,".csv" ,sep="")
       t <-twsSTK(s, exch ="NSE" ,currency="INR")
       d <-reqHistoricalData(tws,t,duration="5 D",file= file.name,barSize="1 min",endDateTime=edt,verbose = TRUE,)
       
       try(data <- read.table(file.name ,header = F, sep = ",") , silent = TRUE)
       
      if(nrow(data)==0)
      {
      cat("TWS_PERMIN_stocks",timestamp(),"UpdatePerminStocks",paste("ERROR IN ",tablename, sep= "") , "\n" , file=connection.log , sep = ",")
     } 
     else 
     {
       data <- cbind(data , VERSION)
       names(data)<-c("TIMESTAMP", "OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT" ,"VERSION")
       ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
      cat("TWS_PERMIN_stocks",timestamp(),"UpdatePerminStocks",paste("Added",tablename,date, sep= " ") , "\n" , file=connection.log , sep = ",")
     }
      
      } 
   }

