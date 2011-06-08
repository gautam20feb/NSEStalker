filterEQ <- function(con,reftable,eqtable)
{
  data <- dbGetQuery(con, paste("select distinct SYMBOL from", reftable, "group by SYMBOL"))
  print(data)
  print(list)
  print(paste("delete from", eqtable, "where SYMBOL not in (",paste("'",data[[1]],"'", collapse = ", ",sep = ""), ")"))
  dbGetQuery(con,paste("delete from", eqtable, "where SYMBOL not in (",paste("'",data[[1]],"'", collapse = ", ",sep = ""), ")"))
    
}