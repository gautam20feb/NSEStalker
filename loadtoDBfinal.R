library(fBasics)
library(fImport)
library (methods)
library(RMySQL)

m <- dbDriver("MySQL")
conn<-dbConnect(
m,
### The name of the connection
dbname = "nsedb",      
### The name of database to connect
user = "root",
### The username for MySQL
password = "intern123", 
### The password for MySQL
host = "localhost")
### The host for MySQL 

mylog2 <- file("log2.csv", "w")  ##<< For logging the files written to database

### Function takes the start date and the end date for the period for which you want to make the 
### the database.
load.to.db<-function()
### The function to run the main process

{
  
  w<-getwd()
  fldrtomysql(conn,w)
  close(mylog)  ##<< Closing the download log connection
  close(mylog2)  ##<< Closing the database connection 
}

### Adding the data from files into the database
fldrtomysql <- function(
### Reades all the .csv files and calls rdtomysql() function
connection,
folderpath)
### folderpath: path of the folder
{
  temp <- getwd()
  setwd(folderpath)
  names <-dir()
  setwd(temp)
  for (name in names)
  {
    
    type<-substr(name,1,2)  ##<< first two letter decide if its a future and options or equities
    string =substr(name,nchar(name)-3,nchar(name))
    if (string == ".csv") 
    {
      if(type=="cm"){
      
      rdtomysql(connection,paste(folderpath,"/",name,sep=""),"equity")
      } 
      if(type=="fo") {
      
      rdtomysql(connection,paste(folderpath,"/",name,sep=""),"fo")
      }
    
   }
  
  }
}
### Adds a csv to the database
rdtomysql <- function(connection, filename, tablename)
{
  data <- read.table(filename,header = T, sep = ",")  ##<<  reads the data plus an extra NULL column
  data$X<- NULL     ##<<  deleting an extra column read

  connection= conn
  ### Writing log - 3 fields - time , type , the file added
  cat(as.character(timestamp()),"adding to DB ", filename, "\n",file = mylog2, sep = ",")
  if(tablename=="equity")
  {
    print(filename)
    ifelse(dbExistsTable(connection, tablename),dbWriteTable(connection, name =tablename, value=data, append = T),dbWriteTable(connection, name = tablename, value=data))
    
  }  
    
  if(tablename=="fo")
  {
      names(data)<-c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTION_TYP","OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH","OPEN_INT","CHG_IN_OI","TIMESTAMP")
    #dataf1<-data[grep("XX", data$OPTION_TYP, ignore.case=T),]  ##<< seperating the rows corresponding to Future
    dataf1<- data[data$STRIKE_PR ==0,]
    print(filename) 
    dataf1$STRIKE_PR<- NULL   ##<< deleting unused column STRIKE_PR from futures 
   dataf1$OPTION_TYP<-NULL   ##<< deleting unused column OPTION_TYP from futures
    datao<- data[data$STRIKE_PR > 0,]  ##<< seperating the rows corresponding to Options
    ifelse(dbExistsTable(connection, "future"),dbWriteTable(connection, name ="future", value=dataf1, append = T),dbWriteTable(connection, name = "future", value=dataf1))
    ifelse(dbExistsTable(connection, "options"),dbWriteTable(connection, name ="options", value=datao, append = T),dbWriteTable(connection, name = "options", value=datao))
  }  
  
}
