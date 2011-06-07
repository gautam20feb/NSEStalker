library(IBrokers)
library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(xts)
tws<-twsConnect(clientId=10)

############################## Getting the list of working day #########################################
holiday<-read.csv("./data/holiday.csv", header = T)
tS = timeSequence(from = "2010-06-08", to = "2011-06-03", by = "day")
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


mylog2 <- file("database.log.csv", "w")  ##<< For logging the files written to database


################################# Connecting to database #########################################
### Defining the type of connection
m <- dbDriver("MySQL", max.con = 25)

### Reading the machines.xml file
doc = xmlRoot(xmlTreeParse("./config/machines.xml")) ##<< parses all of the config file
tmp = xmlSApply(doc , function(x) xmlSApply(x, xmlValue)) ##<< creates a matrix of machines information
tmp = t(tmp) ##<< takes transpose 
n<-nrow(tmp) ##<< get the number of users
machines = as.data.frame(matrix((tmp), n)) ##<< produces a dataframe for the matrix
names(machines) = names(doc[[1]]) ##<< names the corresponding columns

#################################### Initialization ################################################
stk<-read.csv("./data/STK.with.Sym.csv", sep=",", header=T)
no<- nrow(stk)

n<-length(wday)
sequence<-seq(5,n,by=5)

### Function to start the execution 
Tws.per.min.data.to.db<-function(
user.name="intern"
### The default user name is intern
)
  {

  pos=which(machines[,1]==user.name)
  usr=as.character(machines[pos,1])
  hst=as.character(machines[pos,2])
  dbnm=as.character(machines[pos,3])
  pswd=as.character(machines[pos,4])
  conn <- dbConnect(m, user=usr, password = pswd, host = hst, dbname= dbnm) ## sets the connection with the required database
 
  
  for(i in 1:no)  ##<< Loop for different stocks
  {
    
    tablename<-as.character(stk[i,4])
    #Sys.sleep(300)
    for(j in sequence)  ##<< Loop for all the different set of 4 days for any stock
      {
    
       enddatetime<- wday[j]
       temp1<-strsplit(enddatetime, "-")
       temp2<-unlist(temp1)
       date<-paste(temp2[1],temp2[2],temp2[3], sep="")
       print(date)
       Sys.sleep(8)
       s<-stk[i,3]
       s<-as.character(s)
       edt<-paste(date, " 00:00:00",sep="")  ##<< Gives the date in required format
  
       t <-twsSTK(s, exch ="NSE" ,currency="INR")

       d <-reqHistoricalData(tws,t,duration="5 D",file=paste("./per_min_data/",as.character(stk[i,2]),".per.day.min",date,".csv"),barSize="1 min",endDateTime=edt,verbose = TRUE,)
       data <- read.table(paste("./per_min_data/",as.character(stk[i,2]),".per.day.min",date,".csv"),header = F, sep = ",")
       names(data)<-c("TIMESTAMP", "OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT")
      if(nrow(data)==0)
      {
        cat(as.character(timestamp()),paste("added to",tablename," ERROR ",enddatetime,sep=" ") , "\n",file = mylog2, sep = ",")
     } 
     else 
     {
       ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
       cat(as.character(timestamp()),paste("added to",tablename,"data for last 5 days with end date",enddatetime,sep=" ") , "\n",file = mylog2, sep = ",")
     }
      
      } 
   }
}
