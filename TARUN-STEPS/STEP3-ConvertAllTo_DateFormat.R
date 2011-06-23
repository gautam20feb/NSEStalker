library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
# Converts the format of the expiry date and timestamp of all the tables. Earlier, 1-Apr-2011, now 2011-04-01
m <- dbDriver("MySQL", max.con = 100)
ConvertToDateFormat <-function (){
  con <- dbConnect(m, user = "root", password = "intern123", host = "localhost", dbname = "NSE rawdatabase")
  # Format of available date data is dd-mm-yy. This query will change it to default Sql, R format.
  query.futures.timestamp <- paste ("UPDATE future SET TIMESTAMP=STR_TO_DATE(TIMESTAMP,'%d-%M-%Y')")
  dbGetQuery (con, query.futures.timestamp)
  query.futures.expirydt <- paste ("UPDATE future SET EXPIRY_DT=STR_TO_DATE(EXPIRY_DT,'%d-%M-%Y')")
  dbGetQuery (con, query.futures.expirydt)
  query.equity.timestamp <- paste ("UPDATE equity SET TIMESTAMP=STR_TO_DATE(TIMESTAMP,'%d-%M-%Y')")
  dbGetQuery (con, query.equity.timestamp)
  # query.options.timestamp <- paste ("UPDATE options SET TIMESTAMP=STR_TO_DATE(TIMESTAMP,'%d-%M-%Y')")
  # dbGetQuery (con, query.options.timestamp) 
  # query.options.expirydt <- paste ("UPDATE options SET EXPIRY_DT=STR_TO_DATE(EXPIRY_DT,'%d-%M-%Y')")
  # dbGetQuery (con, query.options.expirydt)
}