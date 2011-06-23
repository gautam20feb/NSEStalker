library (RMySQL)

# Load the NSE(.csv) files to Data Base
AddNseCsvFilesToDatabase<-function
### The main function
(user.name = "root@Ophelia"
### name of the machine to which you want to add the data
) {
  library(fImport)
  library (bitops)
  library (methods)
  library(RMySQL)
  library(XML)
 
  driver<- dbDriver("MySQL" , max.con = 1000) ##<< Defining the type of connection

  doc.xml = xmlRoot(xmlTreeParse("../config/machines.xml"))  ##<< parses all of the config file
  xml.matrix = xmlSApply(doc.xml , function(x) xmlSApply(x, xmlValue))  ##<< creates a matrix of machines information
  xml.matrix = t(xml.matrix) ##<< takes transpose of matrix to make it standard
  number.of.users <- nrow(xml.matrix) ##<< get the number of users
  machines = as.data.frame(matrix((xml.matrix), number.of.users)) ##<< produces a dataframe for the matrix
  names(machines) = names(doc.xml[[1]]) ##<< names the corresponding columns
 
  pos=which(machines[,1]==user.name)  ##<< To find which user is using the code and get his info
  usr=unlist(strsplit(user.name,"@"))[1]
  hst=as.character(machines[pos,2])
  pswd=as.character(machines[pos,3])
 
  conn <- dbConnect(driver, user=usr, password = pswd, host = hst, dbname= "TWS_OHLC_stocks") ##<< sets the connection with the required database
  conn1 <- dbConnect(driver, user=usr, password = pswd, host = hst, dbname= "NSE_raw_database") ##<< sets the connection with the required database
 
 }

# File connections for logging "Uncommon Dates" and "Tracking Status"

datelog <- file ("../log/Stk_Differences_in_dates_log.csv", "w")
log.file <- file ("../log/Stk_OHLC_Validation_log.csv", "w")  ##<< For logging the files written to database
error <- file ("../log/Stk_Error_validation_log.csv", "w")

# Initialization of vectors
open.diff <- c()
high.diff <- c()
low.diff <- c()
close.diff <- c()
comp <- c()
common.dates <- c()
diff <- c()

# List of Companies(from TWS databse whcih has both NSE names and TWS names)
nse.companies <- dbGetQuery (conn, "SELECT `NSE_NAME` FROM Stocks")
tws.companies <- dbGetQuery (conn, "SELECT `NAME_USED` FROM Stocks")
# List of Companies(from NSE databse)
nsedb.companies <- dbGetQuery (conn1, "SELECT DISTINCT `SYMBOL`FROM equity")
# List of common companies from two databases
common.companies <- intersect (nse.companies[ ,1], nsedb.companies[ ,1])

# Check for companies mismatch
if (nrow(nse.companies) != length (common.companies)) print("Error : List of Companies from IB is not the same as List of comapnies from NSEDB")

for (j in 1 : length (common.companies)){
  # Vector of dates for which data is available for a given company from TWS databse
  query <- paste ("SELECT TIMESTAMP FROM `", tws.companies[j,1], "`", sep="")
  date.TWS <- dbGetQuery (conn, query)
  # Vector of dates for which data is available for a given company from NSE databse
  query <- paste ("SELECT `TIMESTAMP` FROM `equity`WHERE `SYMBOL` = '", nse.companies[j,1], "'", sep="")
  date.nsedb <- dbGetQuery (conn1, query)
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
     t1 <- system.time (open_nsedb <- dbGetQuery (conn1, query))
     
     # OHLC data from TWSDB
     query <- paste ("SELECT * FROM `", tws.companies[j,1], "` WHERE `TIMESTAMP`='", common.dates[i], "'", sep="")
     t2 <- system.time (open_TWS <- dbGetQuery (conn, query))
     # Differnce of two values (Validation)
     open.diff[i] <- open_nsedb[1,4] - open_TWS[1,3]
     high.diff[i] <- open_nsedb[1,5] - open_TWS[1,4]
     low.diff[i] <- open_nsedb[1,6] - open_TWS[1,5]
     close.diff[i] <- open_nsedb[1,7] - open_TWS[1,6]
     comp[i] <- common.companies[j]
     
     # Logging cited errors during validation
     if(open.diff[i] > 2 || high.diff > 2 || low.diff > 2 || close.diff > 2){
       cat (as.character (j), "Company:", common.companies[j], "Date:", common.dates[i], "Open_diff=", open.diff[i], "High_diff=", high.diff[i], "Low_diff=", low.diff[i], "Close_diff=", close.diff[i], "\n", file = error, sep = ",")
     }
     # Logging for tracking status
     cat (as.character (timestamp ()), "Date", common.dates[i], "Stock", as.character (j), common.companies[j], "CPU Time for SQL Query", "T1=(" , t1, ")  T2=(", t2, ")", "\n", file = log.file, sep = ",")
  }
  # Writing output of validation to csv
  all <- cbind (comp,common.dates, open.diff, high.diff, low.diff, close.diff)
  write.table (all, file = "Stk_OHLC_data_validation.csv", col.names = NA, append = TRUE, sep = ",")
}
dbDisconnect (conn)
dbDisconnect (conn1)