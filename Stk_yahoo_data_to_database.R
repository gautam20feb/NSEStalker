# downloads the daily equity data from the available 181 stocks (having futures) in Yahoo
library (fBasics)    
library (fImport)
library (RMySQL)
futures_list <- read.csv("~/projects/NSEStalker/data/futures_list.csv", sep=";")  
 # imports the file containing symbols compatible with yahoo
m <- dbDriver ("MySQL", max.con = 100)  # Defining driver            
username = "swetha"  # Give phpmyadmin username
con <- dbConnect (m, user = username, password = "intern123", host = "localhost", dbname = "yahoo_data")            
 # Setting up connection. Create yahoo_data in phpmyadmin before running this
mylog2 <- file ("~/projects/NSEStalker/log/yahoo_data_download_log.csv", "w")
 # Writing the status into log file

for(i in 1:nrow(symbols))
try({  # Skips the iteration if it doesn't run
  series.data1<-yahooSeries(as.character(symbols[i,2]),from = "2010-07-01", to= Sys.timeDate())
 # Creating a vector of yahoo series from given date to system date
  series.data<-as.data.frame(series.data1)  # Converting the vector into data frame
  colnames(series.data) <- paste(c("Open", "High", "Low", "Close", "Volume", "A.Close"),sep = ",")
 # Creating header from series.data  

ifelse(dbExistsTable(con,as.character(symbols[i,1])),dbWriteTable(con,symbols[i,1], value=series.data, append = T),dbWriteTable(con,as.character(symbols[i,1]), value=series.data))
 # If file doesn't exist, creates and writes to it. Else it appends
                                                         
cat(as.character(timestamp()),"adding to the yahoo_data table", i, as.character(symbols [i, 1]), "\n", file = mylog2, sep = ",")
  # Tracking the running status of the program
}, silent = TRUE)  # remains silent if some iteration doesn't run
list <- dbListTables (con)  # Lists all the table names in yahoo_data
diff <- setdiff(symbols [, 1], list)  # Shows all those in yahoo symbols list that are not in the vector list