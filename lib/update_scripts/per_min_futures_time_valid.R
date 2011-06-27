library(timeDate)
library(fBasics)
library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(xts)
library(timeDate)
library(gregmisc)


config_path <- "../config/"

### This Script Needs futures_list.csv file in Data folder
### It produces for each stock in the database TWS_per_min_stocks a table having the information about breaks
### Each table has columns : "START_DATE","START_TIME","END_DATE","END_TIME","DIFFERENCE","BREAK_TYPE"
### START_DATE , START_TIME indicate the date,time of start of the interval where there is a break
### START_DATE , START_TIME indicate the date,time of end of the interval where there is a break
### DIFFERENCE gives the length ( End - Start ) in minutes
### BREAK_TYPE indicates the type of the break
### BREAK_TYPE = 0 if the break is due to the end of the day (Natural break)
### BREAK_TYPE = 1 if the break is due to some irregularity in data (Irregular break)
TwsPerminFuturesValidateForTime <- function(
user.name = "intern@Ophelia"
) {
  futures_list<-trim(read.csv("../data/futures_list.csv", sep=",", header=T)) ### Read the Stock Name
  no<-nrow(futures_list)

  machines=GenerateMachimesDataframe(config_path)
  connection.TWS_PERMIN_futures <- CreateConnection(user.name , machines ,"TWS_PERMIN_futures")
  connection.VALIDATION_TWS_PERMIN_futures <- CreateConnection(user.name , machines ,"VALIDATION_TWS_PERMIN_futures")

for(i in 1 :no)
{
  try( if(1==1)
  {
    data <- data.frame()
    stock=as.character(futures_list[i,4])
    query<-paste("SELECT DISTINCT TIMESTAMP FROM " , stock," ORDER by TIMESTAMP ASC" ,sep="")
    tab<-(dbGetQuery(connection.TWS_PERMIN_futures,query)) ## implements the query
    cat("TWS_PERMIN_stocks" ,timestamp(),"TwsPerminFuturesValidateForTime",paste("Read", stock ,sep=" "),"\n",file = connection.log, sep = ",")
    dates<-(tab[1])
    dates.vector<-as.vector(dates[,1])
    date.time<-timeDate(dates.vector, format = "%Y-%m-%d %H:%M:%S")
    dif.in.time<-diff(date.time)
    break.ind <-which(dif.in.time!=1)
    length.break<-as.character(dif.in.time[break.ind])
    length.break2<-as.numeric(length.break)
    type<-ifelse(length.break2>1000,0,1)
    differencedateend<-as.character(date.time[break.ind+1])
    differencedatestart<-as.character(date.time[break.ind])
    start.date<- (substring(differencedatestart,1,10))
    end.date<- (substring(differencedateend,1,10))
    start.time<-(substring(differencedatestart,12,19))
    end.time<-substring(differencedateend,12,19)
    tablename=stock
    d<-cbind(start.date,start.time,end.date,end.time, length.break2,type)
    data<-as.data.frame(d)
    names(data)<-c("START_DATE","START_TIME","END_DATE","END_TIME","DIFFERENCE","BREAK_TYPE")
    str<-ifelse( nrow(data)>0 , "Added" , "Not Added")
    cat("VALIDATION_TWS_PERMIN_stocks" ,timestamp(),"TwsPerminFuturesValidateForTime",paste(str, tablename ,sep=" "),"\n",file = connection.log, sep = ",")
    ifelse(dbExistsTable(connection.VALIDATION_TWS_PERMIN_futures, tablename),dbWriteTable(connection.VALIDATION_TWS_PERMIN_futures, name =tablename, value=data, append = T),dbWriteTable(connection.VALIDATION_TWS_PERMIN_futures, name = tablename, value=data))
  } 
  ,silent=TRUE)
}
close(connection.log)
dbDisconnect(connection.TWS_PERMIN_futures)
dbDisconnect(connection.VALIDATION_TWS_PERMIN_futures)
}