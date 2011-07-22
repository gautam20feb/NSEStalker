library(RMySQL)
library(timeDate)
library(timeSeries)

m <- dbDriver ("MySQL")
  connection.1<-dbConnect(m, dbname = "VALIDATION_NSE_OHLC_futures", user = "intern", password = "intern123", host = "192.168.1.106")
tables <- dbListTables(connection.1)


query <- "SELECT DISTINCT SYMBOL FROM TIME_LINE"
stocks <- dbGetQuery(connection.1,query)
matrix <- data.frame()
for(i in 1 : nrow(stocks))
{
  
  query <- paste("SELECT MIN(BREAK_POINTS),MAX(BREAK_POINTS) FROM BREAK_POINT WHERE SYMBOL LIKE '" , stocks[i,1] ,"'" , sep="")
  dates <- dbGetQuery(connection.1,query)
  query3 <- paste("SELECT START_DATE FROM TIME_LINE WHERE SYMBOL LIKE '" , stocks[i,1] ,"'" , sep="")
  start.date <- dbGetQuery(connection.1,query3)
  query <- paste("SELECT * FROM BREAK_POINT WHERE SYMBOL LIKE '" , stocks[i,1] ,"'" , sep="")
  no.of.points <- nrow(dbGetQuery(connection.1,query))

  min.date <- dates[1]
  max.date <- dates[2]
  
  
  difference <- as.numeric(diff.Date( timeDate(dates))) - no.of.points
  temp <- data.frame(as.character(stocks[i,1]), start.date , min.date , max.date,difference)
  matrix <- rbind(matrix , temp)
}
names(matrix) <- c("SYMBOL", "START_DATE" , "BREAK_START" , "BREAK_END" , "DIFFERENCE")
matrix$DIFFERENCE <- NULL
write.table(matrix , file = "TIMELINE_futures.csv", sep= ",", row.names = FALSE)