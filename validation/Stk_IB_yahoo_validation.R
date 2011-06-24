# Validates OHLC data from IB and Yahoo database
library (fImport)
library (RMySQL)

source("../lib/read_machines.R")
source("../lib/create_connection.R")

connection.log <- file (paste ("../log/", as.character (timestamp()), "_Stk_yahoo_IB_OHLC_validation_log.csv", sep = ""), "w") ##<< For logging the files written to database
datelog<-file(paste("../log/", as.character (timestamp()), "_Stk_IB_yahoo_diff_date_log.csv"),"w")  
config_path<-"../config/"

ValidateIBandYahooDatabse <- function(
### Function to add the stocks daily data to database
user.name = "intern@Ophelia"
### The default user name is intern
){
  
  machines = GenerateMachimesDataframe(config_path)
  ### sets the connection with the required database
  connection.YAHOO_OHLC_stocks <- CreateConnection (user.name , machines ,"YAHOO_OHLC_stocks") 
  connection.TWS_OHLC_stocks <- CreateConnection (user.name , machines ,"TWS_OHLC_stocks")
  
  # Initialization of vectors
  open.diff <- c()
  high.diff <- c()
  low.diff <- c()
  close.diff <- c()
  company <- c()
  common.dates <- c()
  diff <- c()
  
  yahoo.companies <- as.data.frame (dbListTables (connection.YAHOO_OHLC_stocks))
  tws.companies <- dbGetQuery (connection.TWS_OHLC_stocks, "SELECT `NAME_USED` FROM Stocks")
  
  common.companies <- as.data.frame (intersect (yahoo.companies[ ,1], tws.companies[ ,1]))
  
  if (length (yahoo.companies) != length (common.companies)) print ("Error : List of Companies from Yahoo is not the same as List of comapnies from NSEDB")
  for (j in 1 : length (common.companies)) {
    query <- paste ("SELECT `TIMESTAMP` FROM `", yahoo.companies[j,1], "`", sep = "")
    date.yahoo <- dbGetQuery (connection.YAHOO_OHLC_stocks, query)
    
    query <- paste ("SELECT TIMESTAMP FROM `", yahoo.companies[j,1], "`", sep = "")
    date.TWS <- dbGetQuery (connection.TWS_OHLC_stocks, query)
    
    common.dates <- intersect (date.yahoo[ ,1], date.TWS[ ,1])
    diff <- setdiff (date.yahoo[ ,1], date.TWS[ ,1])
    
    cat (as.character (j), common.companies[j,1],diff,"\n",file = datelog, sep = ",")
    
    query <- paste ("SELECT * FROM `", yahoo.companies[j,1], "`", sep = "")
    open.TWS <- dbGetQuery (connection.TWS_OHLC_stocks, query)
     
    query <- paste ("SELECT * FROM `", yahoo.companies[j,1], "`", sep = "")
    open.yahoo <- dbGetQuery (connection.YAHOO_OHLC_stocks, query)
    
    for (i in 1 : length (common.dates)){
      x <- as.character (which (open.TWS$TIMESTAMP == common.dates[i]))
      y <- as.character (which (open.yahoo$row.names == common.dates[i]))
      
      open.diff[i] <- open.TWS[x,3] - open.yahoo[y,2]
      high.diff[i] <- open.TWS[x,4] - open.yahoo[y,3]
      low.diff[i] <- open.TWS[x,5] - open.yahoo[y,4]
      close.diff[i] <- open.TWS[x,6] - open.yahoo[y,5]
      company[i] <- common.companies[j,1]
    }
    cat ("Validating YAHOO_OHLC_stocks and TWS_OHLC_stocks", timestamp(), "Date", common.dates[i], "Stock", as.character (j), common.companies[j], "\n", file = mylog2, sep = ",")
    all <- cbind (company, common.dates, open.diff, high.diff, low.diff, close.diff)
    write.table (all, file = "../log/Stk_IB_yahoo_OHLC_validation_data.csv", col.names = NA, append = TRUE, sep = ",")
  }
  dbDisconnect (connection.YAHOO_OHLC_stocks)
  dbDisconnect (connection.TWS_OHLC_stocks)
}