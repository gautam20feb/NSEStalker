library(timeDate)
library(fBasics)
library(fImport)
library("RCurl")
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(gregmisc)

log_path <- "../log/" ##<< variable
config_path <- "../config/" ##<< varibale
data_path <- "../data/" ##<< variable

gen.machines.dataframe <- function()
### Reads the machines.xml file and returns a dataframe with available machines' information i.e. machine name, user name, database, password, host name
{
doc = xmlRoot(xmlTreeParse(paste(config_path,"machines.xml", sep = ""))) ##<< parses all of the config file
tmp = xmlSApply(doc , function(x) xmlSApply(x, xmlValue)) ##<< creates a matrix of machines information
tmp = t(tmp) ##<< takes transpose 
n<-nrow(tmp) ##<< get the number of users
machines = as.data.frame(matrix((tmp), n)) ##<< produces a dataframe for the matrix
names(machines) = names(doc[[1]]) ##<< names the corresponding columns
machines
}

create.connection <- function
### Creates a MySQL connection 
(machine.name,
### the machine to which the connection is to be made
machines
### dataframe with available machines' information i.e. machine name, user name, database, password, host name
){
m <- dbDriver("MySQL", max.con = 25)

pos=which(machines[,1]==machine.name)
usr=as.character(machines[pos,2])
hst=as.character(machines[pos,3])
dbnm=as.character(machines[pos,4])
pswd=as.character(machines[pos,5])
conn <- dbConnect(m, user=usr, password = pswd, host = hst, dbname= dbnm)
conn
### the created MySQL connection is returned
}

go.db <- function
### The main function, provides the menu to access rest of the functions in the script
(user=""
### the name of the machine to connect to
){
if (user=="")
{
cat("machine name as *user name*@*machine name*")
user <- scan(what = "", nmax = 1)
}

 cat(" 1. Type 1 for downloading from nse and then storing the data as csv files in the ./data/downloaded", "\n",
      "2. Type 2 for importing all the data from given directory to the database storing equities' data in table equity and derivatives' data in table fo ", "\n",
      "3. Type 3 for filtering the equities' table i.e. rejecting data for stocks not having futures", "\n",
      "4. Type 4 for updating", "\n",
      "5. Type 5 to export data from database as csv files", "\n",
      "6. Type 6 to import data form csv to database")
 option <- scan(what = "", nmax = 1)  ##<< The main Menu 
 if (option == 1)
 { 
   cat("enter starting date")
   a <- scan(what = "", nmax = 1)
   cat("enter ending date")
   b <- scan(what = "", nmax = 1)
   get.bhavcopy(a, b, user)
   }
   else if (option == 2)
   {
    read.to.database.all(user)
    }
   else if (option == 3)
   {
    cat("enter refrence future table name")
    reftable <- scan(what = "", nmax = 1)
    cat("enter equity table name to be filtered")
    eqtable <- scan(what = "", nmax = 1)
    filterEQ(user, reftable, eqtable)
    }
    else if (option == 4)
    {}
   else if (option == 5)
   {
    cat("folder to output csv files to")
    folder  <- scan(what = "", nmax = 1)
    dbtocsv(user, folder)
    }
   else if (option == 6)
   {
     cat("folder to take read csv files from")
    folder <- scan(what = "", nmax = 1)
    csvtodb(folder, user)
    }
}

download.to.database<-function
### The function to run the main process i.e. to get data from the nse website and store it in a MySQL database.
(a,
### The starting date in yyyy-mm-dd format
b,
### The ending date in yyyy-mm-dd format
user.name= "root@localhost"
### name of the machine to which you want to add the data
)
{
  conn <- create_connection(user.name) ##<< sets the connection with the required database
  generate.urls(a,b)
  
### For logging downloading using connection
mylog <- file(
paste(log_path, "downloading.log.csv", sep = ""),
### Name of the Log file
"w")  
### writing privileges

mylog2 <- file(paste(log_path, "database.log.csv", sep = ""), "w")  ##<< For logging the files written to database

mylog3<- file(paste(log_path, "error.log.csv", sep = "") , "w")  ##<< For logging the errors

  
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

###################################### Extracting #############################################################  
  ### Calling the extractall function
  dir<-getwd()
  extract.all.files(dir,dir)  
  ### Closing the download log connection
  close(mylog)
  
###################################### Adding to Database ##################################################### 
  read.to.database.all(conn,w)  
  ### Closing the database log connection
  close(mylog2)     
  ### Closing the error connection
  close (mylog3)
  dbDisconnect(conn)
}

get.bhavcopy<-function(
### The function to download the bhavcopies as zip files to the current directory and unzipping them in the same
a,
### The starting date in yyyy-mm-dd format
b)
### The ending date in yyyy-mm-dd format
{
  
  generate.urls(a,b)
  ### For logging downloading using connection
mylog <- file(
paste(log_path, "downloading.log.csv", sep = ""),
### Name of the Log file
"w")  
### writing privileges

mylog3<- file(paste(log_path, "error.log.csv", sep = "") , "w")  ##<< For logging the errors

  
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

###################################### Extracting #############################################################  
  ### Calling the extractall function
  dir<-paste(data_path, "downloaded", sep = "")
  extract.all.files(dir,dir)  
  ### Closing the download log connection
  close(mylog) 
  ### Closing the database log  
  close (mylog3)
  dbDisconnect(conn)
}


################################## The Functions Used #############################################################

### Adding the data from files into the database
### This functions takes saves the contents of all the csv file in the folder folderpath in the table tablename.
read.to.database.all <- function(
user,
### connection with the MySQL server
folderpath = paste(data_path, "downloaded", sep = ""))
### This functions takes saves the contents of all the csv file present in the folder folderpath
{
  machines <- gen.machines.dataframe()
  conn <- create.connection(user, machines)
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
 
  dbDisconnect(conn)
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
  
  mylog2 <- file(paste(log_path, "database.log.csv", sep = ""), "a")  ##<< For logging the files written to database
  
  ### Writing log - 3 fields - time , type , the file added
  
  if(tablename=="equity")
  {
    ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
    cat(as.character(timestamp()),"added to equity ", filename, "\n",file = mylog2, sep = ",")
  }  
    
  if(tablename=="fo")
  {
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
  close(mylog2) 
  
}



### Downloading Equity file(zip) from the URL and saving in the current working directory.
download.FUT.OPT<-function
(sURL)
### The URL of the file to be downloaded
{
### For logging downloading using connection
mylog <- file(
paste(log_path, "downloading.log.csv", sep = ""),
### Name of the Log file
"a")  
### writing privileges

mylog3<- file(paste(log_path, "error.log.csv", sep = "") , "a")  ##<< For logging the errors
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
close(mylog)
close(mylog3)
}



### Downloading Equity file(zip) from the URL and saving in the current working directory.
download.STK<-function
(sURL)
### The URL of the file to be downloaded
{

### For logging downloading using connection
mylog <- file(
paste(log_path, "downloading.log.csv", sep = ""),
### Name of the Log file
"a")  
### writing privileges

mylog3<- file(paste(log_path, "error.log.csv", sep = "") , "a")  ##<< For logging the errors
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
 close(mylog)
close(mylog3)
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

filterEQ<- function
### removes data corresponding to all those stocks which do not have futures.
(user,
## user name to create the connection
reftable = "",
### the reference future tablename from the same database
eqtable)
### name of the equity table to be filtered
{
  machines <-gen.machines.dataframe()
  con <- create.connection(user,machines)
  f<-paste(data_path,"futures_list.csv",sep="")
  futures_list<-trim(read.csv(f, header=F)[,1])
  query<-paste("delete from", eqtable, "where SYMBOL not in (",paste("'",futures_list,"'", collapse = ", ",sep = ""), ")")
  dbGetQuery(con,query)
  query1<-paste("delete from", reftable, "where SYMBOL not in (",paste("'",futures_list,"'", collapse = ", ",sep = ""), ")")
  dbGetQuery(con,query1)
  dbDisconnect(con)  
}

dbtocsv <- function
### the function returns the tables in the given database in the form of csv files, one for each table in the given folder.
(user,
### user name to create connection to the database
folder = getwd())
### folder to which the database is to be copied in the form of csv files
{
    machines <-gen.machines.dataframe()
  con<-create.connection(user, machines)
  tables <- dbListTables(con)
  for (name in tables)
  {
    data <- dbReadTable(con,name,row.names = NULL)    
  write.csv(data,paste(folder, "/", name,".csv", sep = ""), row.names = F)
  }
  dbDisconnect(con)
}

csvtodb <- function
### the functions takes all the csv files in the given folder and stores them as different tables in the given database.
(folder,
### folder which contains the csv files to be moved to database
user)
### user name to create connection to the database
{
    machines <-gen.machines.dataframe()
    con<-create.connection(user, machines)
    temdir <- getwd()
    setwd(folder)
    files <- dir()
    for (name in files)
    {
      temp = unlist(strsplit(name , "\\."))
    tablename = temp[1]
    skip <- 0
    nrows <- 1000
    filename <-paste(folder,"/", name, sep = "")
     res <- try(
       {data <- read.csv(filename,row.names = NULL, nrows = nrows,skip = skip)
      columns <- names(data)
      dbWriteTable(con, tablename, data, row.names = F, append = T)
      skip <- skip + nrows + 1
       }, T)
    while (class(res) != "try-error")
    {
      res <- try(
        {data <- read.csv(filename,row.names = NULL, nrows = nrows,skip = skip, header = F,blank.lines.skip = F)
      names(data) <- columns
      dbWriteTable(con, tablename, data, row.names = F, append = T)
      skip <- skip + nrows + 1
      }, T)
      }
    }
    setwd(temdir)
    dbDisconnect(con)
}

  generate.urls <- function(
  ### generates urls for the bhavcopies corrospoding to a given period and writes them into the csv file named list_of_urls.csv.
  a,
  ### starting date of the period
  b)
  ### ending date
  {
  holiday<-read.csv(paste(data_path, "holiday.csv", sep = ""), header = T)
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
write.csv(x,file=paste(data_path, "list_of_urls.csv", sep = ""))
}
 
