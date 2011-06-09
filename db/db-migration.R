library(RMySQL)
m<- dbDriver("MySQL")
dbtocsv <- function
### the function returns the tables in the given database in the form of csv files, one for each table in the given folder.
(dbname,
### name of the database to be read to csv files
folder = getwd())
### folder to which the database is to be copied in the form of csv files
{
  con<-dbConnect(m, dbname = dbname, user = "root", password = "intern123", host = "localhost")
  tables <- dbListTables(con)
  for (name in tables)
  {
    data <- dbReadTable(con,name,row.names = NULL)    
  write.csv(data,paste(folder, "/", name,".csv", sep = ""), row.names = F)
  }
}

csvtodb <- function
### the functions takes all the csv files in the given folder and stores them as different tables in the given database.
(folder,
### folder which contains the csv files to be moved to database
dbname)
### name of  the database to which the files are to  be moved
{
    con<-dbConnect(m, dbname = dbname, user = "root", password = "intern123", host = "localhost")
    files <- dir()
    for (name in files)
    {
      temp = unlist(strsplit(name , "\\."))
    tablename = temp[1]
      data <- read.csv(paste(folder,"/", name, sep = ""),row.names = NULL)
      dbWriteTable(con, tablename, data, row.names = F)
    }
}