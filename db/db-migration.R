library(RMySQL)
m<- dbDriver("MySQL")
dbtocsv <- function(dbname, folder = getwd())
{
  con<-dbConnect(m, dbname = dbname, user = "root", password = "intern123", host = "localhost")
  tables <- dbListTables(con)
  for (name in tables)
  {
    data <- dbReadTable(con,name,row.names = NULL)    
  write.csv(data,paste(folder, "/", name,".csv", sep = ""), row.names = F)
  }
}

csvtodb <- function(folder,dbname)
{
    con<-dbConnect(m, dbname = dbname, user = "root", password = "intern123", host = "localhost")
    files <- dir()
    for (name in files)
    {
      temp = strsplit(names , ".")
    tablename = temp[1]
      data <- read.csv(paste(folder,"/", name, sep = ""),row.names = NULL)
      dbWriteTable(con, tablename, data, row.names = F)
    }
}