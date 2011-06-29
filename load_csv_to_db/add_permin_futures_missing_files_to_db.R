library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(gregmisc)

source("../lib/read_machines.R")
source("../lib/create_connection.R")
connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database
expiry.dates <- read.csv("../data/nearest_expiry_date.csv", header= T,sep= ",")

config_path <- "../config/"
futures_list<-read.csv("../data/futures_list.csv", sep=",", header=T)

TwsPerminMissingFutureFiles <- function( 
### Adds the permin future missing files 
user.name="intern@Ophelia"
){
  no<- nrow(futures_list)
  machines=GenerateMachimesDataframe(config_path)
  connection.TWS_PERMIN_futures <- CreateConnection(user.name , machines ,"TWS_PERMIN_futures")
  w<-getwd()
  setwd("../data/downloaded/TWS_PERMIN_futures_missing/")
  data.path <- getwd()
  AddAllTwsPerminMissingFiles(connection.TWS_PERMIN_futures,data.path)
  setwd(w)
  close(connection.log)
}

  
AddOneTwsPerminMissingFile <- function
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
  date <-unlist (strsplit(as.character(data$TIMESTAMP)[1]," "))[1]
  index <- (which(expiry.dates[,2] ==  date))
  exp.date <- as.character(expiry.dates[index,3])
  data <- cbind(data,exp.date)
  names(data)<-c("TIMESTAMP","OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT","VERSION","EXPIRY_DT")
  ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
  cat("TWS_PERMIN_futures" ,timestamp(),"AddOneTwsPerminFile",paste("Written future to", tablename ,sep=" "),"\n",file = connection.log, sep = ",")

  }

################################## The Functions Used #############################################################

### This functions takes, saves the contents of all the csv file in the folder folderpath in the table tablename.
AddAllTwsPerminMissingFiles <- function(
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
      n<-strsplit(name,"_")
      n1<-unlist(n)
      n2<-n1[1]
      n3<-trim(substr(n2,1,(nchar(n2)-4)))
      ind<-which(trim(futures_list[,2])==n3)
      AddOneTwsPerminMissingFile(conn,name,as.character(trim(futures_list[ind,4])))
    }
  }
}
