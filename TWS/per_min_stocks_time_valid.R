library(timeDate)
library(fBasics)
library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(xts)
library(timeDate)

### This Script Needs STK.with.Sym.csv file in Data folder
### It produces for each stock in the database TWS_per_min_stocks a table having the information about breaks
### Each table has columns : "START_DATE","START_TIME","END_DATE","END_TIME","DIFFERENCE","BREAK_TYPE"
### START_DATE , START_TIME indicate the date,time of start of the interval where there is a break
### START_DATE , START_TIME indicate the date,time of end of the interval where there is a break
### DIFFERENCE gives the length ( End - Start ) in minutes
### BREAK_TYPE indicates the type of the break
### BREAK_TYPE = 0 if the break is due to the end of the day (Natural break)
### BREAK_TYPE = 1 if the break is due to some irregularity in data (Irregular break)
stk<-read.csv("./data/STK.with.Sym.csv", sep=",", header=T) ### Read the Stock Nam
no<-nrow(stk)
n <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(n, user="gautam", password = "intern123", host = "localhost", dbname="TWS_per_min_stocks")
conn1<- dbConnect(n, user="gautam", password = "intern123", host = "localhost", dbname="TWS_per_min_stocks_time")
mylog2 <- file("./log/validation_time.log.csv", "w")  ##<< For logging the files written to database

for(i in 1:no)
{
  try( if(1==1)
  {
    stock=as.character(stk[i,4])
    query<-paste("SELECT TIMESTAMP FROM " , stock," ORDER by TIMESTAMP ASC" ,sep="")
    tab<-(dbGetQuery(con,query)) ## implements the query
    dt<-(tab[1])
    dtt<-as.vector(dt[,1])
    date.time<-timeDate(dtt, format = "%Y-%m-%d %H:%M:%S")
    dif.in.time<-diff(date.time)
    break.ind <-which(dif.in.time!=1)
    length.break<-as.character(dif.in.time[break.ind])
    length.break2<-as.numeric(length.break)
    type<-ifelse(length.break2>1000,0,1)
    differencedateend<-as.character(date.time[break.ind+1])
    differencedatestart<-as.character(date.time[break.ind])
    sdate<- (substring(differencedatestart,1,10))
    edate<- (substring(differencedateend,1,10))
    stime<-(substring(differencedatestart,12,19))
    etime<-substring(differencedateend,12,19)
    tablename=stock
    d<-cbind(sdate,stime,edate,etime, length.break2,type)
    data<-as.data.frame(d)
    names(data)<-c("START_DATE","START_TIME","END_DATE","END_TIME","DIFFERENCE","BREAK_TYPE")
    str<-ifelse( 
    nrow(data)>0
    ,
    "Added" , "Not Added")
    cat(as.character(timestamp()),paste(str,tablename,sep=" ") , "\n",file = mylog2, sep = ",")
    ifelse(dbExistsTable(conn1, tablename),dbWriteTable(conn1, name =tablename, value=data, append = T),dbWriteTable(conn1, name = tablename, value=data))
  } 
  ,silent=TRUE)
}
close(mylog2)
dbDisconnect(con)
dbDisconnect(conn1)