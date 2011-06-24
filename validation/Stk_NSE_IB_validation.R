# Validates OHLC data from NSE and IB database
library (fImport)
library (RMySQL)

source("../lib/read_machines.R")
source("../lib/create_connection.R")
config_path<-"../config/"

ValidateIBandNSEDatabse <- function(
### Function to add the stocks daily data to database
user.name = "intern@Ophelia"
### The default user name is intern
){
  # File connections for logging "Uncommon Dates" and "Tracking Status"
  datelog <- file (paste("../log/", as.character(timestamp()),"_Stk_Differences_in_dates_log.csv", sep = ""), "w")
  connection.log <- file (paste("../log/", as.character(timestamp()),"_Stk_OHLC_Validation_log.csv", sep = ""), "w")  ##<< For logging the files written to database
  error.log <- file (paste("../log/", as.character(timestamp()),"_Stk_Error_validation_log.csv", sep = ""), "w")
  
  machines = GenerateMachimesDataframe(config_path)
  ### sets the connection with the required database
  connection.NSE_raw_database <- CreateConnection (user.name , machines ,"NSE_raw_database") 
  connection.TWS_OHLC_stocks <- CreateConnection (user.name , machines ,"TWS_OHLC_stocks")
  
  # Initialization of vectors
  open.diff <- c()
  high.diff <- c()
  low.diff <- c()
  close.diff <- c()
  comp <- c()
  common.dates <- c()
  diff <- c()
  
  # List of Companies(from TWS databse whcih has both NSE names and TWS names)
  nse.companies <- dbGetQuery (connection.TWS_OHLC_stocks, "SELECT `NSE_NAME` FROM Stocks")
  tws.companies <- dbGetQuery (connection.TWS_OHLC_stocks, "SELECT `NAME_USED` FROM Stocks")
  # List of Companies(from NSE databse)
  nsedb.companies <- dbGetQuery (connection.NSE_raw_database, "SELECT DISTINCT `SYMBOL`FROM equity")
  # List of common companies from two databases
  common.companies <- intersect (nse.companies[ ,1], nsedb.companies[ ,1])
  # Check for companies mismatch
  if (nrow(nse.companies) != length (common.companies)) print("Error : List of Companies from IB is not the same as List of comapnies from NSEDB")
  for (j in 1 : length (common.companies)){
    # Vector of dates for which data is available for a given company from TWS databse
    query <- paste ("SELECT TIMESTAMP FROM `", tws.companies[j,1], "`", sep="")
    date.TWS <- dbGetQuery (connection.TWS_OHLC_stocks, query)
    # Vector of dates for which data is available for a given company from NSE databse
    query <- paste ("SELECT `TIMESTAMP` FROM `equity`WHERE `SYMBOL` = '", nse.companies[j,1], "'", sep="")
    date.nsedb <- dbGetQuery (connection.NSE_raw_database, query)
    # Intersection of two date vectors
    common.dates <- intersect (date.TWS[ ,1], date.nsedb[ ,1])
    diff <- setdiff (date.TWS[ ,1], date.nsedb[ ,1])
    # Logging for Dates Mismatch in the two databses
    for (k in 1 : length (diff)){
      if (length (which (date.TWS == diff[k])) == 0) remark <- "Present in NSEDB but not present in TWS"
      if (length (which (date.nsedb == diff[k])) == 0) remark <- "Present in TWS but not present in NSEDB"
      cat (as.character (j), "Company:", common.companies[j], "Date:", diff[k], "Remark:", remark, "\n", file = datelog, sep = ",")
    }
    for (i in 1 : length (common.dates)){
      # OHLC data from NSEDB
      query <- paste ("SELECT * FROM `equity`WHERE `TIMESTAMP` = '", common.dates[i], "' AND `SYMBOL` = '", nse.companies[j,1], "'", sep="")
      t1 <- system.time (open_nsedb <- dbGetQuery (connection.NSE_raw_database, query))
      
      # OHLC data from TWSDB
      query <- paste ("SELECT * FROM `", tws.companies[j,1], "` WHERE `TIMESTAMP`='", common.dates[i], "'", sep="")
      t2 <- system.time (open_TWS <- dbGetQuery (connection.TWS_OHLC_stocks, query))
      
      # Differnce of two values (Validation)
      open.diff[i] <- open_nsedb[1,4] - open_TWS[1,3]
      high.diff[i] <- open_nsedb[1,5] - open_TWS[1,4]
      low.diff[i] <- open_nsedb[1,6] - open_TWS[1,5]
      close.diff[i] <- open_nsedb[1,7] - open_TWS[1,6]
      comp[i] <- common.companies[j]
      
      # Logging cited errors during validation
      if(open.diff[i] > 2 || high.diff > 2 || low.diff > 2 || close.diff > 2){
        cat (as.character (j), "Company:", common.companies[j], "Date:", common.dates[i], "Open_diff=", open.diff[i], "High_diff=", high.diff[i], "Low_diff=", low.diff[i], "Close_diff=", close.diff[i], "\n", file = error.log, sep = ",")
      }
      # Logging for tracking status
      cat ("Validating NSE_OHLC_stocks and TWS_OHLC_stocks", timestamp(), "Date", common.dates[i], "Stock", as.character (j), common.companies[j], "CPU Time for SQL Query", "T1=(" , t1, ")  T2=(", t2, ")", "\n", file = connection.log, sep = ",")
    }
    # Writing output of validation to csv
    all <- cbind (comp,common.dates, open.diff, high.diff, low.diff, close.diff)
    write.table (all, file = paste("../log/", as.character(timestamp()),"Stk_OHLC_data_validation_data.csv", sep = ""), col.names = NA, append = TRUE, sep = ",")
  }
  dbDisconnect (connection.TWS_OHLC_stocks)
  dbDisconnect (connection.NSE_raw_database)
  close(datelog)
  close(connection.log)
  close(error.log)
}