library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(gregmisc)

mylog2 <- file("./log/database.log.csv", "w")  ##<< For logging the files written to database

### Defining the type of connection
m <- dbDriver("MySQL", max.con = 25)

### Reading the machines.xml file
stk<- read.csv(file="./data/STK.with.Sym.csv", header=T , sep=",")

conn <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname= "TWS_per_min") ## sets the connection with the required database

w<-getwd()
read.to.database.all(conn,w)
close(mylog2)   


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
  folderpath<-getwd()
  names <-dir()
  setwd(temp)
  for (name in names)
  {
#     Sys.sleep(0.1)
#     name<-names[4]
#     type<-substr(name,1,2)  ##<< first two letter decide if its a future and options or equities
      string =substr(name,nchar(name)-3,nchar(name))
      if (string == ".csv") 
      {
        n<-strsplit(name," ")
        n1<-unlist(n)
        n2<-n1[2]
        n3<-trim(n2)
        ind<-which(stk[,2]==n3)
        read.to.database.one.file(conn,name,stk[ind,4])
      }
  }
}
### This functions saves the data content of the file given by filename to the mysql table tablename of database corresponding to the connection
read.to.database.one.file <- function
(conn,
### connection with the MySQL server
filename,
### filename: name of the csv file or the complete path in case it is not located in your current working directory
tablenametemp)
### tablename: table name in database corrosponding to connection
{
  #   filename<-name
  tablename<-as.character(tablenametemp)
  data <- read.table(filename,header = F, sep = ",")  ##<<  reads the data plus an extra NULL column
  #data$X<- NULL     ##<<  deleting an extra column read
  names(data)<-c("TIMESTAMP","OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","HASGAPS","COUNT")
  ifelse(dbExistsTable(conn, tablename),dbWriteTable(conn, name =tablename, value=data, append = T),dbWriteTable(conn, name = tablename, value=data))
  ### Writing log - 3 fields - time , type , the file added
  cat(as.character(timestamp()),"added to equity ", filename, "\n",file = mylog2, sep = ",")
}