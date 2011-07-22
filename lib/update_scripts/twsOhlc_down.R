library(IBrokers)
library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(xts)
library(timeDate)
library(gregmisc)


#################################### Initialization ################################################

### Function to start the execution 
UpdateTwsOhlc<-function(
### The default user name is intern
connection = connection.temp,
start.date=min.end.date.tws.ohlc.stk,
old.version=0
){
  futures_list <-read.csv("../data/futures_list.csv", sep=",", header=T)
  no<- nrow(futures_list)
  conn <- connection
  end.date <- Sys.Date()
   
  e<- as.timeDate(end.date)
  s <- as.timeDate(start.date)
  days <- difftimeDate(e , s)
  days <- as.character(days)
  
  for(i in 1:no)  ##<< Loop for different stocks
  { 
    data<-data.frame()
    tablename<-trim(as.character(futures_list[i,4]))
#     i=7
       Sys.sleep(8)
       s<-trim(as.character(futures_list[i,3]))
       nse.name=trim(as.character(futures_list[i,2]))
       file.name <- paste("../data/downloaded/update/TWS_OHLC_stocks/",nse.name," .per.day.data_",as.character(end.date),".csv",sep="") 
       t <-twsSTK(s, exch ="NSE" ,currency="INR")
       d <-reqHistoricalData(tws , t , duration = paste(days, " D",sep=""),file = file.name ,barSize="1 day",verbose = TRUE,)
       try(data <- read.table( file.name,header = F, sep = ",") , silent=TRUE)
       
       if(nrow(data) > 0 )
         {
         VERSION <-old.version +1
         data <- cbind(data,VERSION)
         names(data)<-c("TIMESTAMP", "OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT","VERSION")
         ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
         cat("TWS_OHLC_stocks" , timestamp(),"UpdateTwsOhlc", paste("added to",tablename,"daily data" ,sep=" "), "\n",file = connection.log, sep = ",")
         }
    }
   
}