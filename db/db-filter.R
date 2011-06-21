library(gregmisc)
filterEQ<- function
### removes data corresponding to all those stocks which do not have futures.
(con,
## the connection pointing to the mysql database
reftable="future"
,
### the reference future tablename from the same database
eqtable="equity"
)
### name of the equity table to be filtered
{
  futures_list<-trim(read.csv("./data/futures_list.csv", header=T)[,1])
  query<-paste("delete from", eqtable, "where SYMBOL not in (",paste("'",futures_list,"'", collapse = ", ",sep = ""), ")")
  dbGetQuery(con,query)
  query1<-paste("delete from", reftable, "where SYMBOL not in (",paste("'",futures_list,"'", collapse = ", ",sep = ""), ")")
  dbGetQuery(con,query1)
}