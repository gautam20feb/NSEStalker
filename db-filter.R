filterEQ <- function(con,tablename)
{
    print(paste("delete from", tablename, "where 'SYMBOL' not in (", listSTK(), ")"))
    dbSendQuery(con,paste("delete from", tablename, "where SYMBOL not in (", listSTK(), ")"))
    
}