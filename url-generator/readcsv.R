library(RMySQL)
m<- dbDriver("MySQL")
conn<-dbConnect(m, dbname = "Holiday", user = "root", password = "intern123", host = "localhost")

rdtomysql <- function(connection, filename, tablename)
# filename: name of the csv file
# tablename: table name in database corrosponding to connection
{
  data <- read.table(filename,header = F, sep = ",")
  if(dbExistsTable(connection, tablename)){
   dbWriteTable(connection, name =tablename, value=data, append = T)
  }
  else   dbWriteTable(connection, name = tablename, value=data)  
  
}
rdtomysql(conn,"holiday.csv","Hol")