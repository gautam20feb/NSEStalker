library(timeDate)
library(fBasics)
library(fImport)
library("RCurl")
library (bitops)
library (methods)
library(RMySQL)
library(XML)

### For logging downloading using connection
mylog <- file(
"./log/downloading.log.csv",
### Name of the Log file
"w")  
### writing privileges

mylog2 <- file("./log/database.log.csv", "w")  ##<< For logging the files written to database

mylog3<- file("./log/error.log.csv" , "w")  ##<< For logging the errors

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
### The function to run the main process i.e. to get data from the nse website and store it in a MySQL database.
a,
### The starting date in yyyy-mm-dd format
b,
### The starting date in yyyy-mm-dd format
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
 
  holiday<-read.csv("./data/holiday.csv", header = T)
  tS = timeSequence(from = a, to = b, by = "day")

  char<-as.character(tS)
  day<-dayOfWeek(tS)
  trading<-as.character(seq(length(char)))   ##<<  The trading days
  settlement<-as.character(seq(length(char))) ##<<  The settlement days
  reason<-as.character(seq(length(char)))    ##<<  Reason for being a holiday
  mont<-as.character(seq(length(char)))      ##<< Month
  year<-as.character(seq(length(char)))       
  date<-as.character(seq(length(char)))
  equity<-as.character(seq(length(char)))        ##<<  URLs for equities
  derivative<-as.character(seq(length(char)))    ##<<  URLs for derivatives
  atom<-atoms(tS)
  for (i in 1:length(tS))
  {
    if(atom[i,2]==1) mont[i]<-"JAN"
    if(atom[i,2]==2) mont[i]<-"FEB"
    if(atom[i,2]==3) mont[i]<-"MAR"
    if(atom[i,2]==4) mont[i]<-"APR"
    if(atom[i,2]==5) mont[i]<-"MAY"
    if(atom[i,2]==6) mont[i]<-"JUN"
    if(atom[i,2]==7) mont[i]<-"JUL"
    if(atom[i,2]==8) mont[i]<-"AUG"
    if(atom[i,2]==9) mont[i]<-"SEP"
    if(atom[i,2]==10) mont[i]<-"OCT"
    if(atom[i,2]==11) mont[i]<-"NOV"
    if(atom[i,2]==12) mont[i]<-"DEC"
    year[i]<-atom[i,1]
    date[i]<-atom[i,3]
  }
  ### Marking all the Weekends as holidays
  for(i in 1:length(char))    
  {
    if(day[i]=="Sat" || day[i]=="Sun")
    {
      trading[i]<-"Holiday"
      settlement[i]<-"Holiday"
      reason[i]<-"Weekend"
    }
    else
    {
      trading[i]<-"Working Day"
      settlement[i]<-"Working Day"
      reason[i]<-""
    }

  ### Marking all the holidays as as listed in holiday.csv
    for(j in 1:nrow(holiday))     
    {
      if(as.character(char[i]) == as.character(holiday[j,1]))
      {
        trading[i]<-"Holiday"
        settlement[i]<-"Holiday"
        reason[i]<-as.character(holiday[j,3])
      }
    }
  }
### creating URLs 
  for(i in 1:length(tS))
  {
    if(trading[i]=="Working Day"||settlement[i]=="Working Day")  ##<< Creating URLs for working days
    {
      if(as.integer(date[i])<10)   ##<<  Checking for the dates less than 10 and concatinating 0 
      {
        equity[i]<-as.character(composeURL("www.nseindia.com/content/historical/EQUITIES/",year[i],"/",mont[i],"/cm0",date[i],mont[i],year[i],"bhav.csv.zip"))
        derivative[i]<-as.character(composeURL("www.nseindia.com/content/historical/DERIVATIVES/",year[i],"/",mont[i],"/fo0",date[i],mont[i],year[i],"bhav.csv.zip"))
      }
      else
      {
        equity[i]<-as.character(composeURL("www.nseindia.com/content/historical/EQUITIES/",year[i],"/",mont[i],"/cm",date[i],mont[i],year[i],"bhav.csv.zip"))
        derivative[i]<-as.character(composeURL("www.nseindia.com/content/historical/DERIVATIVES/",year[i],"/",mont[i],"/fo",date[i],mont[i],year[i],"bhav.csv.zip"))
      }
    }
    else   ##<< Adding NA to all the holidays
    {
      equity[i]<-NA
      derivative[i]<-NA
    }
  }

################################## Printing ###################################################

all<-cbind(char,day,trading,settlement,reason,date,mont,year,equity,derivative)
x <- matrix(all, nrow =length(char),ncol=10, dimnames = list(c(), c("Date", "Day","For Trading", "For Settlement", "Reason", "Date", "Month","Year","Equity URL","Derivative URL")))
write.csv(x,file="./data/list_of_urls.csv")


################################## Downloading ###################################################

  ### Downloading the zipped files for equitites
  sapply(
  equity,
  ### Vector having the URLs for the equities
  download.STK)

  ### Downloading the zipped files for derivatives
  sapply(derivative,
  ### Vector having the URLs for the derivatives
  download.FUT.OPT)
 

 
  w<-getwd()
###################################### Extracting #############################################################  
  ### Calling the extractall function
  extract.all.files(w,w)  
###################################### Adding to Database ##################################################### 
  read.to.database.all(conn,w)
  
  ### Closing the download log connection
  close(mylog)  
  
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

  
#   if( ( nrow(dataf1)+nrow(dataf2)+nrow(dataf3)+nrow(datao1)+nrow(datao2))!=nrow(data) )
#   {
#     cat(as.character(timestamp()),"  Error in Data ", filename,"\n",file = mylog3, sep = ",")
#   }
#   
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



### Downloading Equity file(zip) from the URL and saving in the current working directory.
download.FUT.OPT<-function
(sURL)
### The URL of the file to be downloaded
{
  #Sys.sleep(poisson())
  tryCatch(if(!is.na(sURL))
  {
    cat(as.character(timestamp()),"  downloading ", sURL,"\n",file = mylog, sep = ",")
    options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6;en-US; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12")
    tContent <- getURLContent(sURL, verbose = TRUE, useragent = getOption("HTTPUserAgent"))
    attributes(tContent) = NULL
    t<-nchar(sURL)
    c<-substr(sURL, t-22 ,t)
    writeBin(tContent, c )    
  },
  error=function(e) {
  cat(as.character(timestamp()),"  URL NOT FOUND ", sURL,"\n",file = mylog3 , sep = ",")
 } )
}



### Downloading Equity file(zip) from the URL and saving in the current working directory.
download.STK<-function
(sURL)
### The URL of the file to be downloaded
{
 # Sys.sleep(poisson())
  tryCatch(if(!is.na(sURL))
  {
    cat(as.character(timestamp()),"downloading ", sURL, "\n",file = mylog, sep = ",")
    options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6;en-US; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12")
    tContent <- getURLContent(sURL, verbose = TRUE, useragent = getOption("HTTPUserAgent"))
    attributes(tContent) = NULL
    t<-nchar(sURL)
    c<-substr(sURL, t-22 ,t)
    writeBin(tContent, c )    
  },
  error=function(e) {
  cat(as.character(timestamp()),"  URL NOT FOUND ", sURL,"\n",file = mylog3, sep = ",")
 } )
 
}



### Gives an integer following the poisson distribution with parameter 4
poisson<- function()
{
  k = 0
  p = 1.0
  L <-exp(-4)
  while (p >= L)  
  {
    k<-k+1
    x1 <- runif(1, 0, 1)
    p <- p*x1
  }
  k<- k-1
  return (k)
}



### This function extracts all zip folders contained in the folder infldrpath to the targetfolder.
extract.all.files <- function
(infldrpath,
### infldrpath: path of the folder including folder name
targetfldrpath)
### targetfldrpath: path of the folder to extract to.
{
  temp <- getwd()
  setwd(infldrpath)
  names <-dir()
  setwd(temp)
  for (name in names)
  {
    string =substr(name,nchar(name)-7,nchar(name))
    if (string == ".csv.zip")  
    extract.one.file(paste(infldrpath,"/",name,sep = ""),targetfldrpath)
  }
}
### This function is our purpose specific because it extracts only the csv file whose name it derives from the zip folder name.
### This function extracts the csv file corresponding to the zip folder name, given by infldrpath, to the folder targetfldrpath.
extract.one.file <- function
(infldrpath,
### infldrpath: the path of the folder containing the zip. The path ends with the name of the zip folder.
 targetfldrpath)
### targetfldrpath: path of the folder to extract to.
{
  pathvector= strsplit(infldrpath, "/")[[1]]
  fldrname = pathvector[length(pathvector)]
  temp <- getwd()
  setwd(substr(infldrpath,1,nchar(infldrpath) - nchar(fldrname)-1))
  filename = substr(fldrname, 1, length(fldrname)-4) # avoiding ".zip"
  zip.file.extract(filename,zipname =fldrname,dir =targetfldrpath)
  setwd(temp)
}
