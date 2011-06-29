library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)

source("../lib/read_machines.R")
source("../lib/create_connection.R")
connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database


config_path <- "../config/"
# Splits the equity into different tables for each stock
SplitEquityIntoTables <-function(
### Function to start the splitting
user.name = "intern@Ophelia"  
### The username@machine to create the connections to
) {
  machines=GenerateMachimesDataframe(config_path)
  connection.NSE_raw_database <- CreateConnection(user.name , machines ,"NSE_raw_database")
  connection.NSE_OHLC_stocks  <- CreateConnection(user.name , machines ,"NSE_OHLC_stocks")
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
  if (stock == "L&T") stock = "LANDT"
  if (stock == "J&KBANK") stock = "JANDKBANK"
  if (stock == "HCL_INSYS") stock= "HCLINSYS"
  dbWriteTable(con1, name = stock, value=stk1,append=T)
  cat("NSE_OHLC-stocks" ,timestamp(),"CreateTable",paste("Written equity to", stock ,sep=" "),"\n",file = connection.log, sep = ",")

  }