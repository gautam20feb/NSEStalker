library(IBrokers)
library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(xts)
tws<-twsConnect(clientId=99)


source("../lib/read_machines.R")
source("../lib/create_connection.R")
connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database
config_path<-"../config/"

connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database

################################# Connecting to database #########################################
### Defining the type of connection
m <- dbDriver("MySQL", max.con = 25)

### Reading the machines.xml file

#################################### Initialization ################################################
stk<-read.csv("../data/STK.with.Sym.csv", sep=",", header=T)
no<- nrow(stk)

### Function gives daily data for last one year
Tws.daily.data.to.db<-function(
user.name = "intern@Ophelia"
### The default user name is intern
)
  {
  machines=GenerateMachimesDataframe(config_path)
  conn <- CreateConnection(user.name , machines ,"TWS_OHLC_stocks")
  
  
  for(i in 1:no)  ##<< Loop for different stocks
  {
    
    tablename<-as.character(stk[i,4])
    
       Sys.sleep(8)
       s<-stk[i,3]
       s<-as.character(s)
       t <-twsSTK(s, exch ="NSE" ,currency="INR")
       d <-reqHistoricalData(tws,t,duration="1 Y",file=paste("../data/downloaded/TWS_OHLC_stocks/",as.character(stk[i,2]),".daily",date,".csv",sep=""),barSize="1 day",verbose = TRUE,)
       data <- read.table(paste("../data/downloaded/TWS_OHLC_stocks/",as.character(stk[i,2]),".daily",date,".csv",sep=""),header = T, sep = ",")
       names(data)<-c("TIMESTAMP", "OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT")
   
       ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
       cat(as.character(timestamp()),paste("added to",tablename,"daily data" ,sep=" "), "\n",file = connection.log, sep = ",")
       } 
   
}