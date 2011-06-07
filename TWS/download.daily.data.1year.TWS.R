library(IBrokers)
library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(xts)
tws<-twsConnect(clientId=99)

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

### Function to start the execution 
Tws.daily.data.to.db<-function(
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
    
       Sys.sleep(8)
       s<-stk[i,3]
       s<-as.character(s)
       t <-twsSTK(s, exch ="NSE" ,currency="INR")
       d <-reqHistoricalData(tws,t,duration="1 Y",file=paste("./daily_data/",as.character(stk[i,2]),".daily",date,".csv",sep=""),barSize="1 day",verbose = TRUE,)
       data <- read.table(paste("./daily_data/",as.character(stk[i,2]),".daily",date,".csv",sep=""),header = T, sep = ",")
       names(data)<-c("TIMESTAMP", "OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT")
   
       ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
       cat(as.character(timestamp()),paste("added to",tablename,"daily data" ,sep=" "), "\n",file = mylog2, sep = ",")
       } 
   
}