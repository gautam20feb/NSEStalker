library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(xts)
library(gregmisc)

source("../lib/read_machines.R")
source("../lib/create_connection.R")
connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database
config_path<-"../config/"
#  Add the stocks daily data to database
AddTwsOhlcToDatabase <- function(
### Function to add the stocks daily data to database
user.name = "intern@Ophelia"
### The default user name is intern
)
{ 
  futures_list<-read.csv("../data/futures_list.csv", sep=",", header=T)
  no<- nrow(futures_list)
  machines=GenerateMachimesDataframe(config_path)
  connection.TWS_OHLC_stocks <- CreateConnection(user.name , machines ,"TWS_OHLC_stocks")
  data<-data.frame()
  for (i in 1:no)  ##<< Loop for different stocks
  {
    
    tablename<-trim(as.character(futures_list[i,4]))
    try(
      data <- read.table(paste("../data/downloaded/TWS_OHLC_stocks/",trim(as.character(futures_list[i,2]))," .per.day.data",".csv",sep=""),header = T, sep = ",")
       ,silent=TRUE)
    if (nrow(data))
    {
      VERSION <- 0
      data <- cbind(data,VERSION)
      names(data) <- c("TIMESTAMP","OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT","VERSION")
      ifelse (dbExistsTable(connection.TWS_OHLC_stocks, tablename),dbWriteTable(connection.TWS_OHLC_stocks, name =tablename, value=data, append = T),dbWriteTable(connection.TWS_OHLC_stocks, name = tablename, value=data))
      cat("TWS_OHLC_stocks" , timestamp(),"AddTwsOhlcToDatabase" ,paste("added to",tablename,"daily data" ,sep=" "), "\n",file = connection.log, sep = ",")
    }
    else
    { 
      cat("TWS_OHLC_stocks" , timestamp(),"AddTwsOhlcToDatabase" ,paste("not added to",tablename,"daily data" ,sep=" "), "\n",file = connection.log, sep = ",")
    }
    data<-data.frame()
  }    
}