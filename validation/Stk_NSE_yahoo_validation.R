# Validates OHLC data from NSE and Yahoo database
library (fImport)
library (RMySQL)

source("../lib/read_machines.R")
source("../lib/create_connection.R")
config_path<-"../config/"

ValidateYahooandNSEDatabse <- function(
### Function to add the stocks daily data to database
user.name = "intern@Ophelia"
### The default user name is intern
){
  # File connections for logging "Uncommon Dates" and "Tracking Status"
  datelog <- file (paste("../log/", as.character(timestamp()),"_yahoo_NSE_date_difference_log.csv", sep = ""), "w")
  connection.log <- file (paste("../log/", as.character(timestamp()),"_yahoo_NSE_OHLC_validation_log.csv", sep = ""), "w")
  error <- file (paste("../log/", as.character(timestamp()),"_Stk_Error_validation_log.csv", sep = ""), "w")
  
  connection.YAHOO_OHLC_stocks <- dbConnect(driver, user = usr, password = pswd, host = hst, dbname = "YAHOO_OHLC_stocks")
  connection.NSE_raw_database <- dbConnect(driver, user = usr, password = pswd, host = hst, dbname = "NSE_raw_database")
  
  # Initialization of vectors
  open.diff <- c()
  high.diff <- c()
  low.diff <- c()
  close.diff <- c()
  company <- c()
  common.dates <- c()
  diff <- c()
  
  yahoo.companies <- as.data.frame (dbListTables(connection.YAHOO_OHLC_stocks))
  ##<< gets required names from database
  nsedb.companies <- dbGetQuery (connection.NSE_raw_database, "SELECT DISTINCT `SYMBOL`FROM equityback")
  ##<< sets the connection with the required database
  common.companies <- as.data.frame (intersect (yahoo_companies[, 1], nsedb.companies[, 1]))
  ##<< common companies from yahoo and nse 
  
  if (length(yahoo.companies) != length(common.companies)) print("Error : List of Companies from Yahoo is not the same as List of comapnies from NSEDB")
  for(j in 1:length(common.companies)){
    # Vector of dates for which data is available for a given company from Yahoo database
    query <- paste ("SELECT `TIMESTAMP` FROM `", yahoo.companies[j, 1], "`", sep = "")     
    date.yahoo <- dbGetQuery (connection.YAHOO_OHLC_stocks, query)
    
    # Vector of dates for which data is available for a given company from NSE databse
    query <- paste ("SELECT `TIMESTAMP` FROM `NSE_OHLC_stocks`WHERE `SYMBOL` = '", yahoo.companies[j, 1], "`", sep = "")
    date.nsedb <- dbGetQuery (connection.NSE_raw_database, query)
    
    # Intersection of two date vectors
    common.dates <- intersect (date.yahoo[, 1], date.nsedb[, 1])
    diff <- setdiff (date.yahoo[, 1], date.nsedb[, 1])
    
    # Logging for Dates Mismatch in the two databses
    for (k in 1 : length (diff)){
      if (length (which (date.yahoo == diff[k])) == 0) remark <- "Present in NSEDB but not present in Yahoo"
      if (length (which (date.nsedb == diff[k])) == 0) remark <- "Present in Yahoo but not present in NSEDB"
      cat (as.character (j), "Company:", common.companies[j], "Date:", diff[k], "Remark:", remark, "\n", file = datelog, sep = ",")
    }
    
    # OHLC data from NSEDB
    query <- paste("SELECT * FROM `NSE_OHLC_stocks`WHERE `SYMBOL` = '", yahoo.companies[j], "'", sep = "")
    open.nsedb <- dbGetQuery(connection.NSE_raw_database, query)
    
    # OHLC data from NSEDB
    query <- paste ("SELECT * FROM `", yahoo.companies[j], "`", sep = "")
    open.yahoo <- dbGetQuery (connection.YAHOO_OHLC_stocks, query)
    
    for(i in 1:length(common.dates)){
      x <- as.character(which(open.nsedb$TIMESTAMP == common.dates[i]))
      y <- as.character(which(open.yahoo$TIMESTAMP == common.dates[i]))
      # Differnce of two values (Validation)
      open.diff[i] <- open.nsedb[x, 4]-open.yahoo[y, 2]
      high.diff[i] <- open.nsedb[x, 5]-open.yahoo[y, 3]
      low.diff[i] <- open.nsedb[x, 6]-open.yahoo[y, 4]
      close.diff[i] <- open.nsedb[x,.7]-open.yahoo[y, 5]
      company[i] <- common.companies[j]
      cat ("Validating YAHOO_OHLC_stocks and NSE_OHLC_stocks", timestamp(), "Date", common.dates[i], "Stock", as.character (j), common.companies[j], "\n", file = connection.log, sep = ",")
    }
    # Logging cited errors during validation
    if(open.diff[i] > 2 || high.diff > 2 || low.diff > 2 || close.diff > 2){
      cat (as.character (j), "Company:", common.companies[j], "Date:", common.dates[i], "Open_diff=", open.diff[i], "High_diff=", high.diff[i], "Low_diff=", low.diff[i], "Close_diff=", close.diff[i], "\n", file = error, sep = ",")
    }
    all <- cbind (company, common.dates, open.diff, high.diff, low.diff, close.diff)
    write.table(all, file = "validation.csv", col.names = NA, append = TRUE, sep = ",")
  }
  dbDisconnect (connection.YAHOO_OHLC_stocks)
  dbDisconnect (connection.NSE_raw_database)
  close(datelog)
  close(connection.log)
  close(error)
}