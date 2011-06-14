library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)

mylog2 <- file("database.log.csv", "w")  ##<< For logging the files written to database

mylog3<- file("error.log.csv" , "w")  ##<< For logging the errors

### Defining the type of connection
m <- dbDriver("MySQL", max.con = 25)

### Reading the machines.xml file
doc = xmlRoot(xmlTreeParse("./config/machines.xml")) ##<< parses all of the config file
tmp = xmlSApply(doc , function(x) xmlSApply(x, xmlValue)) ##<< creates a matrix of machines information
tmp = t(tmp) ##<< takes transpose 
n<-nrow(tmp) ##<< get the number of users
machines = as.data.frame(matrix((tmp), n)) ##<< produces a dataframe for the matrix
names(machines) = names(doc[[1]]) ##<< names the corresponding columns
 

### Function takes the start date and the end date for the period for which you want to make the database.
get.bhavcopy<-function(
user.name="intern"
### name of the machine to which you want to add the data
)
{
  pos=which(machines[,1]==user.name)
  usr=as.character(machines[pos,1])
  hst=as.character(machines[pos,2])
  dbnm=as.character(machines[pos,3])
  pswd=as.character(machines[pos,4])
  conn <- dbConnect(m, user=usr, password = pswd, host = hst, dbname= dbnm) ## sets the connection with the required database
 
 
###################################### Adding to Database ##################################################### 
  w<-getwd()
  read.to.database.all(conn,w)
  

  ### Closing the database log connection
  close(mylog2)   
  
  ### Closing the error connection
  close (mylog3)
}


################################## The Functions Used #############################################################

### Adding the data from files into the database
### This functions takes saves the contents of all the csv file in the folder folderpath in the table tablename.
read.to.database.all <- function(
conn,
### connection with the MySQL server
folderpath)
### This functions takes saves the contents of all the csv file present in the folder folderpath
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
      
      read.to.database.one.file(conn,paste(folderpath,"/",name,sep=""),"equity")
      } 
      if(type=="fo") {
      
      read.to.database.one.file(conn,paste(folderpath,"/",name,sep=""),"fo")
      }
    
   }
  
  }
}


### This functions saves the data content of the file given by filename to the mysql table tablename of database corresponding to the connection
read.to.database.one.file <- function
(conn,
### connection with the MySQL server
filename,
### filename: name of the csv file or the complete path in case it is not located in your current working directory
tablename)
### tablename: table name in database corrosponding to connection
{
  data <- read.table(filename,header = T, sep = ",")  ##<<  reads the data plus an extra NULL column
  data$X<- NULL     ##<<  deleting an extra column read
  
  
  ### Writing log - 3 fields - time , type , the file added
  
  if(tablename=="equity")
  {
    ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
    cat(as.character(timestamp()),"added to equity ", filename, "\n",file = mylog2, sep = ",")
  }  
    
  if(tablename=="fo")
  {
  names(data)<-c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTION_TYP","OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH","OPEN_INT","CHG_IN_OI","TIMESTAMP")
  datao1<-data[grep("OPTIDX", data$INSTRUMENT, ignore.case=T),]
  datao2<-data[grep("OPTSTK", data$INSTRUMENT, ignore.case=T),]
  dataf1<-data[grep("FUTIDX", data$INSTRUMENT, ignore.case=T),]
  dataf2<-data[grep("FUTINT", data$INSTRUMENT, ignore.case=T),]
  dataf3<-data[grep("FUTSTK", data$INSTRUMENT, ignore.case=T),]

  
  if( ( nrow(dataf1)+nrow(dataf2)+nrow(dataf3)+nrow(datao1)+nrow(datao2))!=nrow(data) )
  {
    cat(as.character(timestamp()),"  Error in Data ", filename,"\n",file = mylog3, sep = ",")
  }
  
  #dataf<- data[data$STRIKE_PR ==0,]  ##<< seperating the rows corresponding to Future
  #datao<- data[data$STRIKE_PR >0,]  ##<< seperating the rows corresponding to Options
  if(nrow(datao1)>0) {
  ifelse(dbExistsTable(conn, "options"),dbWriteTable(conn, name ="options", value=datao1, append = T),dbWriteTable(conn, name = "options", value=datao1))
  cat(as.character(timestamp()),"added Options Index to options ", filename, "\n",file = mylog2, sep = ",")
  }
   if(nrow(datao2)>0) {
  ifelse(dbExistsTable(conn, "options"),dbWriteTable(conn, name ="options", value=datao2, append = T),dbWriteTable(conn, name = "options", value=datao2))
  cat(as.character(timestamp()),"added Options Stock to options ", filename, "\n",file = mylog2, sep = ",")
  }
  
  if(nrow(dataf1)>0) {
   dataf1$STRIKE_PR<- NULL   ##<< deleting unused column STRIKE_PR from futures 
   dataf1$OPTION_TYP<-NULL   ##<< deleting unused column OPTION_TYP from futures
   ifelse(dbExistsTable(conn, "future"),dbWriteTable(conn, name ="future", value=dataf1, append = T),dbWriteTable(conn, name = "future", value=dataf1))
  cat(as.character(timestamp()),"added Future Index to table Future ", filename, "\n",file = mylog2, sep = ",")
  }
  if(nrow(dataf2)>0) {
  dataf2$STRIKE_PR<- NULL   ##<< deleting unused column STRIKE_PR from futures 
  dataf2$OPTION_TYP<-NULL   ##<< deleting unused column OPTION_TYP from futures
  ifelse(dbExistsTable(conn, "future"),dbWriteTable(conn, name ="future", value=dataf2, append = T),dbWriteTable(conn, name = "future", value=dataf2))
  cat(as.character(timestamp()),"added Future Int to table Future ", filename, "\n",file = mylog2, sep = ",")
  }
  if(nrow(dataf3)>0) {
  dataf3$STRIKE_PR<- NULL   ##<< deleting unused column STRIKE_PR from futures 
  dataf3$OPTION_TYP<-NULL   ##<< deleting unused column OPTION_TYP from futures
  ifelse(dbExistsTable(conn, "future"),dbWriteTable(conn, name ="future", value=dataf3, append = T),dbWriteTable(conn, name = "future", value=dataf3))
  cat(as.character(timestamp()),"added Future Stock to table future ", filename, "\n",file = mylog2, sep = ",")
  }
  
  
  }  
  
}
