library(RMySQL)

source("../lib/read_machines.R")
source("../lib/create_connection.R")

connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database

config_path <- "../config/"

# Removes the duplicate entries from all the tables of the database
RemoveDuplicateEntriesFromDatabase <- function(
dbname ,
### Name of the database to connect
user.name = "intern@Ophelia" 
) {
  machines=GenerateMachimesDataframe(config_path)
  connection.1 <- CreateConnection(user.name , machines ,dbname)
  tables <- dbListTables(connection.1)
  sapply(tables, function(x) { RemoveDuplicateEntriesFromTable(connection.1,x,dbname)  })
}

RemoveDuplicateEntriesFromTable <- function(
### Removes the duplicate entries from the table
connection,
### The Connection to the database
tablename,
### Name of the table to access
dbname
){
  fields <- dbListFields(connection, tablename)
  row.names.index<-which(fields=="row_names")
  
  if( row.names.index > 0 )
  {
    fields<- fields[(row.names.index+1) : length(fields)]
  }
  query2 <- paste ( "SELECT DISTINCT ", paste(fields, collapse = ",",sep = ""), " FROM ", tablename ,sep="")
  data <- dbGetQuery(connection,query2)
  cat(dbname ,timestamp(),"RemoveDuplicateEntriesFromTable",paste("Read Distinct from", tablename ,sep=" "),"\n",file = connection.log, sep = ",")

  query3<- paste("DELETE FROM ", tablename,sep="")
  dbGetQuery(connection,query3)
  cat(dbname ,timestamp(),"RemoveDuplicateEntriesFromTable",paste("Deleted from", tablename ,sep=" "),"\n",file = connection.log, sep = ",")
 
  ifelse(dbExistsTable(connection, tablename),dbWriteTable(connection, name =tablename, value=data, append = T),dbWriteTable(connection, name = tablename, value=data))
  cat(dbname ,timestamp(),"RemoveDuplicateEntriesFromTable",paste("Written Distict from", tablename ,sep=" "),"\n",file = connection.log, sep = ",")

}
  