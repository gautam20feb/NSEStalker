library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)

source("../lib/read_machines.R")
source("../lib/create_connection.R")
  
connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database
config_path <- "../config/"

# Load the NSE(.csv) files to Data Base
NseCsvFilesToDatabase<-function(
### The main function
user.name = "intern@Ophelia",
### name of the machine to which you want to add the data
path.to.nse.files = "../data/downloaded/NSE_rawdb/"
) {
  machines=GenerateMachimesDataframe(config_path)
  connection.NSE_raw_database <- CreateConnection(user.name , machines ,"NSE_raw_database")
  
  current.working.directory<-getwd()
  setwd(path.to.nse.files)
  NSE.data.directory<-getwd()
  AddAllNseFilesToDatabase(connection.NSE_raw_database,NSE.data.directory)
  setwd(current.working.directory)
  
  ### Closing the log connection
  close(connection.log)   

}


################################## The Functions Used #############################################################

AddAllNseFilesToDatabase <- function(
### This functions takes saves the contents of all the csv file in the folder folderpath in the table tablename.
conn ,
### connection with the MySQL server
folderpath
### This functions takes saves the contents of all the csv file present in the folder folderpath
){
  names <-dir()
  for (name in names)
  {
    
    type<-substr(name,1,2)  ##<< first two letter decide if its a future and options or equities
    string =substr(name,nchar(name)-3,nchar(name))
    if (string == ".csv") 
    {
      if (type=="cm"){
      
      AddOneNseFileToDatabase(conn,paste(folderpath,"/",name,sep=""),"equity")
      } 
      if (type=="fo") {
      
      AddOneNseFileToDatabase(conn,paste(folderpath,"/",name,sep=""),"fo")
      }
    
   }
  
  }
}


AddOneNseFileToDatabase <- function(
### This functions saves the data content of the file given by filename to the mysql table tablename of database corresponding to the connection
conn ,
### connection with the MySQL server
filename ,
### filename: name of the csv file or the complete path in case it is not located in your current working directory
tablename
### tablename: table name in database corrosponding to connection
){
  data <- read.table(filename,header = T, sep = ",")  ##<<  reads the data plus an extra NULL column
  data$X<- NULL     ##<<  deleting an extra column read
  VERSION<- 0
  data<- cbind(data,VERSION)
  if (tablename =="equity")
  {
    ifelse(dbExistsTable(conn , tablename),dbWriteTable(conn, name = tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
    cat("NSE_raw_database", timestamp(),"AddOneNseFileToDatabase",paste("added to equity", filename,sep=" ") , "\n",file = connection.log, sep = ",")
  }  
    
  if (tablename =="fo")
  {
  names(data) <-c("INSTRUMENT", "SYMBOL" , "EXPIRY_DT" , "STRIKE_PR" , "OPTION_TYP" , "OPEN" , "HIGH" , "LOW", "CLOSE" , "SETTLE_PR" , "CONTRACTS" , "VAL_INLAKH" , "OPEN_INT" , "CHG_IN_OI" , "TIMESTAMP" , "VERSION")
  datao1 <-data[grep("OPTIDX" , data$INSTRUMENT, ignore.case=T),]
  datao2 <-data[grep("OPTSTK" , data$INSTRUMENT, ignore.case=T),]
  dataf1 <-data[grep("FUTIDX" , data$INSTRUMENT, ignore.case=T),]
  dataf2 <-data[grep("FUTINT" , data$INSTRUMENT, ignore.case=T),]
  dataf3 <-data[grep("FUTSTK" , data$INSTRUMENT, ignore.case=T),]
      
  if (nrow(datao1) > 0) {
  ifelse(dbExistsTable(conn, "options"),dbWriteTable(conn, name ="options", value=datao1, append = T),dbWriteTable(conn, name = "options", value=datao1))
  cat("NSE_raw_database",  timestamp(),"AddOneNseFileToDatabase",paste("added Options Index to options", filename,sep=" "), "\n",file = connection.log, sep = ",")
  }
  if (nrow(datao2) > 0) {
  ifelse(dbExistsTable(conn, "options"),dbWriteTable(conn, name ="options", value=datao2, append = T),dbWriteTable(conn, name = "options", value=datao2))
  cat("NSE_raw_database", timestamp(),"AddOneNseFileToDatabase", paste("added Options Stocks to options" , filename,sep=" "), "\n",file = connection.log, sep = ",")
  }
  
  if (nrow(dataf1) > 0) {
   dataf1$STRIKE_PR<- NULL   ##<< deleting unused column STRIKE_PR from futures 
   dataf1$OPTION_TYP<-NULL   ##<< deleting unused column OPTION_TYP from futures
   ifelse(dbExistsTable(conn, "future"),dbWriteTable(conn, name ="future", value=dataf1, append = T),dbWriteTable(conn, name = "future", value=dataf1))
  cat("NSE_raw_database", timestamp(),"AddOneNseFileToDatabase", paste("added Future Index to table Future", filename , sep=" ") , "\n",file = connection.log, sep = ",")
  }
  if (nrow(dataf2) > 0) {
  dataf2$STRIKE_PR<- NULL   ##<< deleting unused column STRIKE_PR from futures 
  dataf2$OPTION_TYP<-NULL   ##<< deleting unused column OPTION_TYP from futures
  ifelse(dbExistsTable(conn, "future"),dbWriteTable(conn, name ="future", value=dataf2, append = T),dbWriteTable(conn, name = "future", value=dataf2))
  cat("NSE_raw_database", timestamp(),"AddOneNseFileToDatabase",paste("added Future Int to table Future", filename , sep=" ") , "\n",file = connection.log, sep = ",")
  }
  if (nrow(dataf3) > 0) {
  dataf3$STRIKE_PR<- NULL   ##<< deleting unused column STRIKE_PR from futures 
  dataf3$OPTION_TYP<-NULL   ##<< deleting unused column OPTION_TYP from futures
  ifelse(dbExistsTable(conn, "future"),dbWriteTable(conn, name ="future", value=dataf3, append = T),dbWriteTable(conn, name = "future", value=dataf3))
  cat("NSE_raw_database", timestamp(),"AddOneNseFileToDatabase", paste("added Future Stocks to table Future", filename , sep=" ") , "\n",file = connection.log, sep = ",")
  }
  }
}
