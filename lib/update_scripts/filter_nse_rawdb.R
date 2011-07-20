library(gregmisc)
 
config_path <- "../config/"

# Filters the not needed data
Filter <- function(
### Removes data corresponding to all those stocks which do not have futures.
### It needs the future_list.csv file
future.table="future",
### the future tablename from the same database
equity.table="equity",
connection

### the equity tablename from the same database
) {
  connection.NSE_raw_database <- connection.temp
  
  
  futures_list<-trim(read.csv("../data/futures_list.csv", header=T)[,2])
  query<-paste("delete from", equity.table, "where SYMBOL not in (",paste("'",futures_list,"'", collapse = ", ",sep = ""), ")")
  dbGetQuery(connection.NSE_raw_database,query)
  cat("NSE_raw_database" ,timestamp(),"Filter",paste("DELETING stocks without future" , sep=" "),"\n",file = connection.log, sep = ",")

  query1<-paste("delete from", future.table, "where SYMBOL not in (",paste("'",futures_list,"'", collapse = ", ",sep = ""), ")")
  dbGetQuery(connection.NSE_raw_database,query1)
  cat("NSE_raw_database" ,timestamp(),"Filter",paste("DELETING futures without future(any wrong entry)" , sep=" "),"\n",file = connection.log, sep = ",")
}