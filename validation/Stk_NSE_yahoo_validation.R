# Load the NSE(.csv) files to Data Base
AddNseCsvFilesToDatabase <- function
### The main function
(user.name = "root@Ophelia"
### name of the machine to which you want to add the data
) {
  library (fImport)
  library (bitops)
  library (methods)
  library (RMySQL)
  library (XML)
 
  driver <- dbDriver("MySQL" , max.con = 1000) ##<< Defining the type of connection

  doc.xml = xmlRoot(xmlTreeParse("../config/machines.xml"))  ##<< parses all of the config file
  xml.matrix = xmlSApply(doc.xml , function(x) xmlSApply(x, xmlValue))  ##<< creates a matrix of machines information
  xml.matrix = t(xml.matrix)  ##<< takes transpose of matrix to make it standard
  number.of.users <- nrow(xml.matrix)  ##<< get the number of users
  machines = as.data.frame(matrix((xml.matrix), number.of.users)) ##<< produces a dataframe for the matrix
  names (machines) = names (doc.xml[[1]]) ##<< names the corresponding columns
 
  pos = which (machines[, 1] == user.name)  ##<< To find which user is using the code and get his info
  usr = unlist (strsplit(user.name, "@"))[1]
  hst = as.character(machines[pos, 2])
  pswd = as.character(machines[pos, 3])
 
conn <- dbConnect(driver, user = usr, password = pswd, host = hst, dbname = "YAHOO_OHLC_stocks")
conn1 <- dbConnect(driver, user = usr, password = pswd, host = hst, dbname = "NSE_raw_database")
}

# File connections for logging "Uncommon Dates" and "Tracking Status"

datelog <- file ("../log/yahoo_NSE_date_difference_log.csv", "w")
mylog2 <- file ("../log/yahoo_NSE_OHLC_validation_log.csv", "w")  ##<< For logging the files written to database
error <- file ("../log/Stk_Error_validation_log.csv", "w")

 # Initialization of vectors
open.diff <- c()
high.diff <- c()
low.diff <- c()
close.diff <- c()
company <- c()
common.dates <- c()
diff <- c()

yahoo.companies <- as.data.frame (dbListTables(conn))
 ##<< gets required names from database
nsedb.companies <- dbGetQuery (conn1, "SELECT DISTINCT `SYMBOL`FROM equityback")
  ##<< sets the connection with the required database
common.companies <- as.data.frame (intersect (yahoo_companies[, 1], nsedb.companies[, 1]))
 ##<< common companies from yahoo and nse 

if (length(yahoo.companies) != length(common.companies)) print("Error : List of Companies from Yahoo is not the same as List of comapnies from NSEDB")
for(j in 1:length(common.companies))
 {
    # Vector of dates for which data is available for a given company from Yahoo database
   query <- paste ("SELECT `TIMESTAMP` FROM `", yahoo.companies[j, 1], "`", sep = "")     
   date.yahoo <- dbGetQuery (conn, query)

    # Vector of dates for which data is available for a given company from NSE databse
   query <- paste ("SELECT `TIMESTAMP` FROM `NSE_OHLC_stocks`WHERE `SYMBOL` = '", yahoo.companies[j, 1], "`", sep = "")
   date.nsedb <- dbGetQuery (conn1, query)
  
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
     open.nsedb <- dbGetQuery(conn1, query)
     
      # OHLC data from NSEDB
     query <- paste ("SELECT * FROM `", yahoo.companies[j], "`", sep = "")
     open.yahoo <- dbGetQuery (conn, query)
   
   for(i in 1:length(common.dates))
   {
    x <- as.character(which(open.nsedb$TIMESTAMP == common.dates[i]))
    y <- as.character(which(open.yahoo$TIMESTAMP == common.dates[i]))
     
     # Differnce of two values (Validation)
     open.diff[i] <- open.nsedb[x, 4]-open.yahoo[y, 2]
     high.diff[i] <- open.nsedb[x, 5]-open.yahoo[y, 3]
     low.diff[i] <- open.nsedb[x, 6]-open.yahoo[y, 4]
     close.diff[i] <- open.nsedb[x,.7]-open.yahoo[y, 5]
     company[i] <- common.companies[j]
   }
   
     # Logging cited errors during validation
     if(open.diff[i] > 2 || high.diff > 2 || low.diff > 2 || close.diff > 2){
       cat (as.character (j), "Company:", common.companies[j], "Date:", common.dates[i], "Open_diff=", open.diff[i], "High_diff=", high.diff[i], "Low_diff=", low.diff[i], "Close_diff=", close.diff[i], "\n", file = error, sep = ",")
 
   
   all <- cbind (company, common.dates, open.diff, high.diff, low.diff, close.diff)
   write.table(all, file = "validation.csv", col.names = NA, append = TRUE, sep = ",")
}

dbDisconnect (conn)
dbDisconnect (conn1)