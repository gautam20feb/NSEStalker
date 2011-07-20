library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(gregmisc)


source("../lib/read_machines.R")
source("../lib/create_connection.R")
connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database

config_path <- "../config/"
user.name = "intern@Ophelia"
machines=GenerateMachimesDataframe(config_path)

CreateMetadataDatabase <- function(
user.name = "intern@Ophelia"
)
{
  conn1 <- CreateConnection(user.name , machines ,"METADATA_DATABASE")
connNos  <- CreateConnection(user.name , machines ,"NSE_OHLC_stocks")
connNof <- CreateConnection(user.name , machines ,"NSE_OHLC_futures")
connTos<- CreateConnection(user.name , machines ,"TWS_OHLC_stocks")
connTpf <- CreateConnection(user.name , machines ,"TWS_PERMIN_futures")
connTps <- CreateConnection(user.name , machines ,"TWS_PERMIN_stocks")

  
 stocks<-read.csv("../data/futures_list.csv", sep=",", header=T)

  noNos=c()
  noNof=c()
  noTos=c()
#   noNos<-c()
  noTps=c()
  noTpf=c()
  
  for(i in 1:nrow(stocks))
  {
  curr.stk<-trim(as.character(stocks$TABLE_NAME[i]))
  query<- paste("SELECT max(TIMESTAMP) from ", curr.stk  ,sep="")
  query2<- paste("SELECT MIN(VERSION) from ", curr.stk  ,sep="")
  curr.time<- as.character(Sys.time())
  
  print(curr.stk)
  
  
    
  if(
    dbExistsTable(connNos, curr.stk)
    )
  {
  endNos<-dbGetQuery(connNos,query)
  version <-  as.character(dbGetQuery(connNos,query2))
  data<-cbind(curr.stk,endNos, curr.time,version)
  names(data)<- c("TABLE_NAME","END_DATE","LAST_UPDATED_DATE_TIME","VERSION")
  tablename="NSE_OHLC_stocks"
  ifelse(dbExistsTable(conn1, tablename),dbWriteTable(conn1, name =tablename, value=data, append = T),dbWriteTable(conn1, name = tablename, value=data))
  cat("METADATA_DATABASE", timestamp(),"CreateMetadataDatabase", paste("Created metadata for",curr.stk,"of",tablename,sep=" "),"\n", file = connection.log)  
  }
  else noNos<-c(noNos,curr.stk)
    if(dbExistsTable(connNof, curr.stk))
  {
  endNof<-dbGetQuery(connNof,query)
  version <-  as.character(dbGetQuery(connNof,query2))
  data<-cbind(curr.stk,endNof, curr.time,version)
  names(data)<- c("TABLE_NAME","END_DATE","LAST_UPDATED_DATE_TIME","VERSION")
  tablename="NSE_OHLC_futures"
  ifelse(dbExistsTable(conn1, tablename),dbWriteTable(conn1, name =tablename, value=data, append = T),dbWriteTable(conn1, name = tablename, value=data))
  cat("METADATA_DATABASE", timestamp(),"CreateMetadataDatabase", paste("Created metadata for",curr.stk,"of",tablename,sep=" "),"\n", file = connection.log)  
  }
  else noNof<-c(noNof,curr.stk)  
  
  if(dbExistsTable(connTos, curr.stk))
  {
  endTos<-dbGetQuery(connTos,query)
  version <-  as.character(dbGetQuery(connNof,query2))
  data<-cbind(curr.stk,endTos, curr.time, version)
  names(data)<- c("TABLE_NAME","END_DATE","LAST_UPDATED_DATE_TIME","VERSION")
  tablename="TWS_OHLC_stocks"
  ifelse(dbExistsTable(conn1, tablename),dbWriteTable(conn1, name =tablename, value=data, append = T),dbWriteTable(conn1, name = tablename, value=data))
  cat("METADATA_DATABASE", timestamp(),"CreateMetadataDatabase", paste("Created metadata for",curr.stk,"of",tablename,sep=" "),"\n", file = connection.log)  

  }
    else noTos<-c(noTos,curr.stk)
    
    
#   if(dbExistsTable(connTof, curr.stk))
#   {
#   endTof<-dbGetQuery(connTof,query)
#     version <-  as.character(dbGetQuery(connTof,query2))

#   data<-cbind(curr.stk,endTof, curr.time,version)
#   names(data)<- c("TABLE_NAME","END_DATE","LAST_UPDATED_DATE_TIME","VERSION")
#   tablename="TWS_OHLC_futures"
#   ifelse(dbExistsTable(conn1, tablename),dbWriteTable(conn1, name =tablename, value=data, append = T),dbWriteTable(conn1, name = tablename, value=data))
#   }
#   else noTof<-c(noTof,curr.stk)
  if(
    dbExistsTable(connTps, curr.stk)
    )
  {
    
  endTps<-as.data.frame(substr(dbGetQuery(connTps,query),1,10))
  version <-  as.character(dbGetQuery(connTps,query2))
  data<-cbind(curr.stk,endTps, curr.time,version)
  names(data)<- c("TABLE_NAME","END_DATE","LAST_UPDATED_DATE_TIME","VERSION")
  tablename="TWS_PERMIN_stocks"
  ifelse(dbExistsTable(conn1, tablename),dbWriteTable(conn1, name =tablename, value=data, append = T),dbWriteTable(conn1, name = tablename, value=data))
  cat("METADATA_DATABASE", timestamp(),"CreateMetadataDatabase", paste("Created metadata for",curr.stk,"of",tablename,sep=" "),"\n", file = connection.log)  
  
  }
  else noTps<-c(noTps,curr.stk)
  
  if(dbExistsTable(connTpf, curr.stk))
  {
  endTpf<-as.data.frame(substr(dbGetQuery(connTpf,query),1,10))
  version <-  as.character(dbGetQuery(connTpf,query2))
  data<-cbind(curr.stk,endTpf, curr.time,version)
  names(data)<- c("TABLE_NAME","END_DATE","LAST_UPDATED_DATE_TIME","VERSION")
  tablename="TWS_PERMIN_futures"
  ifelse(dbExistsTable(conn1, tablename),dbWriteTable(conn1, name =tablename, value=data, append = T),dbWriteTable(conn1, name = tablename, value=data))
  cat("METADATA_DATABASE", timestamp(),"CreateMetadataDatabase", paste("Created metadata for",curr.stk,"of",tablename,sep=" "),"\n", file = connection.log)  

  }
  else noTpf<-c(noTpf,curr.stk)

  
 }
  write.csv(as.data.frame(noNos),file="no_NSE_OHLC_stocks.csv")
  write.csv(as.data.frame(noTos),file="no_TWS_OHLC_stocks.csv")
  write.csv(as.data.frame(noTps),file="no_TWS_PERMIN_stocks.csv")
  write.csv(as.data.frame(noTpf),file="no_TWS_PERMIN_futures.csv")
  

}