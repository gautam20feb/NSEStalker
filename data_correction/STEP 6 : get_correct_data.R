library(gregmisc)
futures_list <- trim(read.csv("futures_list.csv", sep=","))
tables <-trim(as.character(futures_list$TABLE_NAME))
m<- dbDriver("MySQL")
conn<-dbConnect(m, dbname = "ADJUSTMENT_FACTORS", user = "intern", password = "intern123", host = "192.168.1.106")
conn1<-dbConnect(m, dbname = "NSE_OHLC_futures", user = "intern", password = "intern123", host = "192.168.1.106")
conn2<-dbConnect(m, dbname = "try", user = "intern", password = "intern123", host = "192.168.1.106")
dbListTables(conn1)
CalculateCorrectData <- function()
{
  for(it in 1: length(tables))
  {
    table <- tables[it]
    
    start<-"2001-01-01"
    end<-"2011-08-01"
    query<-paste("SELECT `TIMESTAMP`,`SYMBOL`,OPEN,HIGH,LOW,CLOSE,SETTLE_PR,CONTRACTS,VAL_INLAKH,OPEN_INT,CHG_IN_OI, min(EXPIRY_DT) FROM ", table , " WHERE `TIMESTAMP` BETWEEN '",start,"' AND '",end,"' GROUP BY TIMESTAMP, SYMBOL ORDER BY SYMBOL, TIMESTAMP", sep = "")
    data<-dbGetQuery(conn1,query)
    n <- nrow(data)
    query <- paste("SELECT * FROM ", table , " WHERE ADJ_FACTOR_MUL != '0' Order BY TIMESTAMP ",sep = "")
    adj <- dbGetQuery(conn,query)[1:n,]
    correct.close <- data$CLOSE * adj$ADJ_FACTOR_MUL + adj$ADJ_FACTOR_ADD
    correct.high <- data$HIGH * adj$ADJ_FACTOR_MUL + adj$ADJ_FACTOR_ADD
    correct.low <- data$LOW * adj$ADJ_FACTOR_MUL + adj$ADJ_FACTOR_ADD
    correct.open <- data$OPEN * adj$ADJ_FACTOR_MUL + adj$ADJ_FACTOR_ADD
    correct.settle <- data$SETTLE_PR * adj$ADJ_FACTOR_MUL + adj$ADJ_FACTOR_ADD
 
#     plot(correct,type='l')
#     dbWriteTable(conn ,name=tables[it],value=data)
    data.final <- data.frame(data[,1] ,
                             data$SYMBOL , 
                             adj$ADJ_FACTOR_MUL,
                             adj$ADJ_FACTOR_ADD ,
                             data$CLOSE ,
                             correct.close ,
                             correct.open,correct.high,correct.low,
                             correct.settle,
                             data$CONTRACTS ,
                             data$VAL_INLAKH ,
                             data$OPEN_INT , 
                             data$CHG_IN_OI ,
                             data[ , 12]
                             )
    
    names(data.final) <- c("TIMESTAMP","SYMBOL","ADJ_FACTOR_MUL","ADJ_FACTOR_ADD","OLD_CLOSE","CLOSE","OPEN","HIGH","LOW","SETTLE_PR","CONTRACTS","VAL_INLAKH","OPEN_INT","CHG_IN_OI", "EXPIRY_DT")
    dbWriteTable(conn2 ,name=table,value=data.final)
  }
}
 s