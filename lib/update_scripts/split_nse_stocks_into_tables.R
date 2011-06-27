library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)

# Splits the equity into different tables for each stock
SplitEquityIntoTables <-function(
### Function to start the splitting
connectiontoread,
connectiontowrite
) {
  connection.NSE_raw_database <- connectiontoread
  connection.NSE_OHLC_stocks  <- connectiontowrite
  query1<-paste("SELECT DISTINCT SYMBOL FROM equity ",sep="")
  tables<-dbGetQuery(connection.NSE_raw_database,query1) ##<< The list of all the tables that are there in equity
  tables<-tables[,1]
  assign("tables", tables, envir=globalenv())
  sapply(tables,function(x) { CreateTable(x , connection.NSE_raw_database , connection.NSE_OHLC_stocks  )})
  close(connection.log)
}
CreateTable <- function(
### Create a table from the 
stock,
### Name of the stock
con,
### Connection to database to be read
con1
### Connection to database to be written
) { 
  query <- paste("SELECT * FROM equity WHERE SYMBOL = '" , stock ,"' ORDER BY SYMBOL,TIMESTAMP",sep="")
  stk1 <- dbGetQuery(con, query)
  cat("NSE_raw_database" ,timestamp(),"CreateTable",paste("Reading equity from", stock , sep=" "),"\n",file = connection.log, sep = ",")

  stk1$row_names <- NULL
  row.names(stk1) <- seq(length(stk1[, 1]))
  if (stock == "BAJAJ-AUTO") stock = "BAJAJAUTO"
  if (stock == "M&M") stock = "MANDM"
  if (stock == "MCDOWELL-N") stock = "MCDOWELLN"
  if (stock == "TV-18")stock = "TV18"
  if (stock == "AREVAT&D")stock = "AREVATANDD"
  if (stock == "IT&T") stock = "ITANDT"
  if (stock == "L&T") stock = "LT"
  if (stock == "J&KBANK") stock = "JANDKBANK"
  if (stock == "HCL_INSYS") stock= "HCLINSYS"
  dbWriteTable(con1, name = stock, value=stk1,append=T)
  cat("NSE_OHLC-stocks" ,timestamp(),"CreateTable",paste("Written equity to", stock ,sep=" "),"\n",file = connection.log, sep = ",")

  }