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
futures_list<-trim(read.csv("../data/futures_list.csv", sep=",", header=T))

TwsPerminStocksFiles <- function( 
### Adds the permin future files 
user.name = "intern@Ophelia"  ){
  no<- nrow(futures_list)
  machines=GenerateMachimesDataframe(config_path)
  connection.TWS_PERMIN_stocks <- CreateConnection(user.name , machines ,"TWS_PERMIN_stocks")
  w<-getwd()
  setwd("../data/downloaded/TWS_PERMIN_stocks/")
  data.path <- getwd()
  AddAllTwsPerminFiles(connection.TWS_PERMIN_stocks,data.path)
  setwd(w)
  close(connection.log)
}

AddOneTwsPerminFile <- function
(conn,
### connection with the MySQL server
filename,
### filename: name of the csv file or the complete path in case it is not located in your current working directory
tablename)
### tablename: table name in database corrosponding to connection
{
  data <- read.table(filename,header = F, sep = ",")  ##<<  reads the data plus an extra NULL column
  data$X<- NULL     ##<<  deleting an extra column read
  VERSION<- 0
  data<-cbind(data,VERSION)
  names(data)<-c("TIMESTAMP","OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT","VERSION")
  ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
  cat("TWS_PERMIN_stocks" ,timestamp(),"AddOneTwsPerminFile",paste("Written Stocks to", tablename ,sep=" "),"\n",file = connection.log, sep = ",")

  }

################################## The Functions Used #############################################################

### This functions takes, saves the contents of all the csv file in the folder folderpath in the table tablename.
AddAllTwsPerminFiles <- function(
conn,
### connection with the MySQL server
folderpath)
### This functions takes, saves the contents of all the csv file present in the folder folderpath
{
  names <-dir()
  for (name in names)
  {
    string =substr(name,nchar(name)-3,nchar(name))
    if (string == ".csv") 
    {
      n<-strsplit(name," ")
      n1<-unlist(n)
      n2<-n1[2]
      n3<-trim(n2)
      ind<-which(futures_list[,2]==n3)
      AddOneTwsPerminFile(conn,name,as.character(futures_list[ind,4]))
    }
  }
}
