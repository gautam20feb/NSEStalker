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
  
conn<-dbConnect(driver, user=usr, password = pswd, host = hst, dbname= "YAHOO_OHLC_stocks")
  ##<< sets the connection with the required database
conn1<-dbConnect(driver, user=usr, password = pswd, host = hst, dbname= "TWS_OHLC_stocks")
  ##<< sets the connection with the required database
}
 
datelog<-file("../log/Stk_IB_yahoo_diff_date_log.csv","w")  # 
mylog2 <- file("../log/Stk_yahoo_IB_OHLC_validation_log.csv","w")  ##<< For logging the files written to database

# Initialization of vectors
open.diff <- c()
high.diff <- c()
low.diff <- c()
close.diff <- c()
company <- c()
common.dates <- c()
diff <- c()

yahoo.companies <- as.data.frame (dbListTables (conn))
tws.companies <- dbGetQuery (conn1, "SELECT `NAME_USED` FROM Stocks")

common.companies <- as.data.frame (intersect (yahoo.companies[ ,1], tws.companies[ ,1]))

if (length (yahoo.companies) != length (common.companies)) print ("Error : List of Companies from Yahoo is not the same as List of comapnies from NSEDB")
for (j in 1 : length (common.companies)) {
  query <- paste ("SELECT `TIMESTAMP` FROM `", yahoo.companies[j,1], "`", sep = "")
  date.yahoo <- dbGetQuery (conn, query)
   
  query <- paste ("SELECT TIMESTAMP FROM `", yahoo.companies[j,1], "`", sep = "")
  date.TWS <- dbGetQuery (conn1, query)
  
  common.dates <- intersect (date.yahoo[ ,1], date.TWS[ ,1])
  diff <- setdiff (date.yahoo[ ,1], date.TWS[ ,1])
   
  cat (as.character (j), common.companies[j,1],diff,"\n",file = datelog, sep = ",")
   
  query <- paste ("SELECT * FROM `", yahoo.companies[j,1], "`", sep = "")
  open.TWS <- dbGetQuery (conn1, query)
     
  query <- paste ("SELECT * FROM `", yahoo.companies[j,1], "`", sep = "")
  open.yahoo <- dbGetQuery (conn, query)
   
  for (i in 1 : length (common.dates)){
    x <- as.character (which (open.TWS$TIMESTAMP == common.dates[i]))
    y <- as.character (which (open.yahoo$row.names == common.dates[i]))
     
    open.diff[i] <- open.TWS[x,3] - open.yahoo[y,2]
    high.diff[i] <- open.TWS[x,4] - open.yahoo[y,3]
    low.diff[i] <- open.TWS[x,5] - open.yahoo[y,4]
    close.diff[i] <- open.TWS[x,6] - open.yahoo[y,5]
    company[i] <- common.companies[j,1]
   }
   
   cat (as.character (timestamp()), "Date", common.dates[i], "Stock", as.character (j), common.companies[j], "\n", file = mylog2, sep = ",")
   
   all <- cbind (company, common.dates, open.diff, high.diff, low.diff, close.diff)
   write.table (all, file = "../log/Stk_IB_yahoo_OHLC_validation_data.csv", col.names = NA, append = TRUE, sep = ",")
}
dbDisconnect (conn)
dbDisconnect (conn1)