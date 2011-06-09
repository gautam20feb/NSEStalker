filterEQ<- function
### removes data corresponding to all those stocks which do not have futures.
(con,
## the connection pointing to the mysql database
reftable,
### the reference future tablename from the same database
eqtable)
### name of the equity table to be filtered
{
  data <- dbGetQuery(con, paste("select distinct SYMBOL from", reftable, "group by SYMBOL"))
  print(paste("delete from", eqtable, "where SYMBOL not in (",paste("'",data[[1]],"'", collapse = ", ",sep = ""), ")"))
  dbGetQuery(con,paste("delete from", eqtable, "where SYMBOL not in (",paste("'",data[[1]],"'", collapse = ", ",sep = ""), ")"))
    
}