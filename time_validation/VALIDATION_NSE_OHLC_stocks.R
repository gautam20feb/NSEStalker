library(timeDate)
library(fBasics)
library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(xts)
library(timeDate)

source("../lib/read_machines.R")
source("../lib/create_connection.R")
connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database

config_path <- "../config/"

### This Script Needs futures_list.csv and output.csv file in Data folder
### It produces for each stock in the database NSE_OHLC_stocks a table having the information about breaks
### There are 2 tables TIME_LINE & BREAK_POINTS
### TIME_LINE tells about the start date , end date and the existence of any break
### BREAK_POINTS gives the breaks if there are any
NseOhlcStocksValidateForTime <- function(
user.name = "intern@Ophelia"
) {
  futures_list<-trim(read.csv("../data/futures_list.csv", sep=",", header=T)) ##<< Read the Stock Name
  all.days<-read.csv<-read.csv("../data/output.csv", sep=",", header=T)
  days<- all.days[grep("Working Day", all.days$For.Trading, ignore.case=T),]
  days<-as.character(days[,2])

  no<-nrow(futures_list)
  
  machines=GenerateMachimesDataframe(config_path)
  connection.NSE_OHLC_stocks <- CreateConnection(user.name , machines ,"NSE_OHLC_stocks")
  connection.VALIDATION_NSE_OHLC_stocks <- CreateConnection(user.name , machines ,"VALIDATION_NSE_OHLC_stocks")



  for(i in 1 :no)
  {
    try( if(1==1)
    {
    
    stock=as.character(futures_list[i,4])
    query<-paste("SELECT DISTINCT TIMESTAMP FROM " , stock," ORDER by TIMESTAMP ASC" ,sep="")
    tab<-(dbGetQuery(connection.NSE_OHLC_stocks,query)) ## implements the query
    cat("NSE_OHLC_stocks" ,timestamp(),"NseOhlcStocksValidateForTime",paste("Read", stock ,sep=" "),"\n",file = connection.log, sep = ",")
    print(stock)
    dates<-(tab[1])
    dates.vector<-as.vector(dates[,1])
    Start.date<-dates.vector[1]
    End.date<-tail(dates.vector, n=1L)
    Start.ind<- which(days==Start.date)
    End.ind<- which(days==End.date)
    working.days<- days[Start.ind:End.ind]
    a<-timeSeries(dates.vector)
    b<-timeSeries(working.days)
    difference<-setdiff(b,a)
    flag<-ifelse(length(difference)>0,1 ,0)
    
    tablename="TIME_LINE"
    d<-cbind(stock,Start.date,End.date,flag)
    data<-as.data.frame(d)
    names(data)<-c("SYMBOL","START_DATE","END_DATE","IF_BREAK")
    str<-ifelse( nrow(data)>0,"Added" , "Not Added")
    ifelse(dbExistsTable(connection.VALIDATION_NSE_OHLC_stocks, tablename),dbWriteTable(connection.VALIDATION_NSE_OHLC_stocks, name =tablename, value=data, append = T),dbWriteTable(connection.VALIDATION_NSE_OHLC_stocks, name = tablename, value=data))
    
    print(nrow(data))
    if (flag > 0)
    {
     tablename="BREAK_POINT"
     d<-cbind(stock,Start.date,End.date,difference) 
     data<-as.data.frame(d)
     names(data)<-c("SYMBOL","START_DATE","END_DATE","BREAK_POINTS")
     str<-ifelse( 
     nrow(data)>0
     ,
     "Added" , "Not Added")
     cat("VALIDATION_NSE_OHLC_stocks" ,timestamp(),"NseOhlcStocksValidateForTime",paste(str, tablename ,sep=" "),"\n",file = connection.log, sep = ",")
     ifelse(dbExistsTable(connection.VALIDATION_NSE_OHLC_stocks, tablename),dbWriteTable(connection.VALIDATION_NSE_OHLC_stocks, name =tablename, value=data, append = T),dbWriteTable(connection.VALIDATION_NSE_OHLC_stocks, name = tablename, value=data))
     print(nrow(data))
    }
    
    
    } 
  ,silent=TRUE)
  }
close(connection.log)
dbDisconnect(connection.NSE_OHLC_stocks)
dbDisconnect(connection.VALIDATION_NSE_OHLC_stocks)
}