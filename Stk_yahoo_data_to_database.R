# downloads the daily equity data from the available 181 stocks (having futures) in Yahoo
library (fBasics)    
library (fImport)
library (RMySQL)
library (timeSeries)
library (timeDate)

source("../lib/read_machines.R")
source("../lib/create_connection.R")
config_path<-"../config/"

AddYahooToDatabse <- function(
### Function to add the stocks daily data to database
user.name = "intern@Ophelia"
### The default user name is intern
){
  connection.log <- file (paste ("../log/", as.character (timestamp()), "_yahoo_data_download_log.csv", sep = ""), "w") ##<< For logging the files written to database
  machines = GenerateMachimesDataframe(config_path)
  connection.YAHOO_OHLC_stocks <- CreateConnection (user.name , machines ,"YAHOO_OHLC_stocks") # Setting up connection. Create yahoo_data in phpmyadmin before running this
  
  futures_list <- read.csv("../data/futures_list.csv", sep=",") # imports the file containing symbols compatible with yahoo
  symbols <- futures_list
  
  for(i in 1 : nrow (symbols))
    try({  # Skips the iteration if it doesn't run
      series.data1 <- yahooSeries (as.character (symbols[i,6]), from = "2010-07-01", to = Sys.timeDate())
      ### Creating a vector of yahoo series from given date to system date
      series.data<-as.data.frame(series.data1)  # Converting the vector into data frame
      names(series.data) <- paste(c("OPEN", "HIGH", "LOW", "CLOSE", "VOLUME", "ADJ_CLOSE"),sep = ",")
      ### Creating header from series.data  
      ifelse(dbExistsTable(connection.YAHOO_OHLC_stocks,as.character(symbols[i,4])),dbWriteTable(connection.YAHOO_OHLC_stocks,symbols[i,4], value=series.data, append = T),dbWriteTable(connection.YAHOO_OHLC_stocks,as.character(symbols[i,4]), value=series.data))
      ### If file doesn't exist, creates and writes to it. Else it appends
      cat ("YAHOO_OHLC_stocks",timestamp(), "AddYahooToDatabse","adding to the yahoo_data table", i, as.character(symbols [i,1]), "\n", file = connection.log, sep = ",")
      ### Tracking the running status of the program
    }, silent = TRUE)  ##<< remains silent if some iteration doesn't run
  list <- dbListTables (connection.YAHOO_OHLC_stocks)  ##<< Lists all the table names in yahoo_data
  diff <- setdiff(symbols [ ,4], list)  ##<< Shows all those in yahoo symbols list that are not in the vector list
  print(list)
  print(diff)
  dbDisconnect(connection.YAHOO_OHLC_stocks)
  close(connection.log)
  }