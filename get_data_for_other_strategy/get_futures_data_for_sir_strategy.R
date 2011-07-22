library(RMySQL)
library(gregmisc)


source("../lib/read_machines.R")
source("../lib/create_connection.R")
 
# Get Futures Data having all the data points as Nifty for the time period mentioned in the following format
# TIMESTAMP HIGH LOW CLOSE HIGH LOW CLOSE ....
# this data is in data.csv file
# stock.csv has the names of stocks in order as the data in data.csv
GetFuturesData <- function()
{
config_path <- "../config/"
user.name = "intern@Ophelia"
machines=GenerateMachimesDataframe(config_path)
connection.NSE_raw_database <- CreateConnection(user.name , machines ,"CORRECTED_OHLC_futures_Backward_Without_Div")

start <- "2008-07-18"
end <- "2011-07-20"
table <- "NIFTY"
query <- paste("SELECT MIN(EXPIRY_DT) ,SYMBOL,TIMESTAMP , HIGH , LOW , CLOSE FROM " , table , " WHERE TIMESTAMP BETWEEN '", start , "' AND '", end,"' GROUP BY TIMESTAMP" ,sep="")  
data <- dbGetQuery(connection.NSE_raw_database , query)
names.of.data <- c("TIMESTAMP")
n= nrow(data)
fut_list <- trim(read.csv("../data/futures_list.csv" , sep = "," , header = T))
no.of.rows <- nrow(fut_list)
main <- data.frame(data$TIMESTAMP)
stocks <- c()
for(i in 6: no.of.rows)
{
  table <- as.character(fut_list$TABLE_NAME[i])
  query <- paste("SELECT MIN(EXPIRY_DT) ,SYMBOL,TIMESTAMP , HIGH , LOW , CLOSE FROM " , table , " WHERE TIMESTAMP BETWEEN '", start , "' AND '", end,"' GROUP BY TIMESTAMP" ,sep="")  
  data <- dbGetQuery(connection.NSE_raw_database , query)
  if( nrow(data)== n)
  {
  print(table)
  data$HIGH <- ifelse(data$HIGH > data$CLOSE/2 , data$HIGH , data$CLOSE)
  data$LOW <- ifelse(data$LOW > data$CLOSE/2 , data$CLOSE , data$CLOSE)
  main <- cbind(main , data$HIGH , data$LOW , data$CLOSE)
  names.of.data <- c(names.of.data, "HIGH", "LOW" , "CLOSE")
  stocks <- c(stocks , table)
  }
  names(main) <- names.of.data
  write.table(main , file = "data.csv", sep= "," , row.names = FALSE)
  write.table(stocks , file = "stocks.csv", sep= "," , row.names = FALSE)
  
}
}