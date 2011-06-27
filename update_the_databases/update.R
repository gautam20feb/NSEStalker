library(RCurl)
library(gregmisc)
library(IBrokers)



source("../lib/read_machines.R")
source("../lib/create_connection.R")
source("../lib/update_scripts/create_metadata.R")
source("../lib/update_scripts/filter_nse_rawdb.R")
source("../lib/update_scripts/get_nearest_expiry_date.R")
source("../lib/update_scripts/per_min_futures_time_valid.R")
source("../lib/update_scripts/per_min_stocks_time_valid.R")
source("../lib/update_scripts/split_nse_futures_into_tables.R")
source("../lib/update_scripts/split_nse_stocks_into_tables.R")
source("../lib/update_scripts/twsOhlc_down.R")
source("../lib/update_scripts/update_nse_to_temp.R")
source("../lib/update_scripts/VALIDATION_TWS_OHLC_stocks.R")
source("../lib/update_scripts/VALIDATION_NSE_OHLC_stocks.R")
source("../lib/update_scripts/VALIDATION_NSE_OHLC_Futures.R")
source("../lib/update_scripts/update_tws_permin_stocks.R")
source("../lib/update_scripts/update_tws_permin_future.R")
source("../lib/update_scripts/remove_tables.R")

connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database

config_path <- "../config/"

# tws<-twsConnect(clientId=1)


UpdateAllDatabases <- function(
### Update all the databases
user.name="intern@Ophelia"
### User Name and machine name

)
  {
  machines=GenerateMachimesDataframe(config_path)
  connection.METADATA_DATABASE <- CreateConnection(user.name , machines ,"METADATA_DATABASE")
  futures_list<-trim(read.csv("../data/futures_list.csv", sep=",", header=T))
  no<- nrow(futures_list)
  connection.temp<- CreateConnection(user.name , machines ,"temp")
  connection.NSE_OHLC_futures <- CreateConnection(user.name , machines ,"NSE_OHLC_futures")
  connection.NSE_OHLC_stocks <- CreateConnection(user.name , machines ,"NSE_OHLC_stocks")
  connection.TWS_OHLC_stocks <- CreateConnection(user.name , machines ,"TWS_OHLC_stocks")
  connection.TWS_PERMIN_stocks <- CreateConnection(user.name , machines ,"TWS_PERMIN_stocks")
  connection.TWS_PERMIN_futures <- CreateConnection(user.name , machines ,"TWS_PERMIN_futures")
  connection.VALIDATION_NSE_OHLC_stocks <- CreateConnection(user.name , machines ,"VALIDATION_NSE_OHLC_stocks")
  connection.VALIDATION_NSE_ONLC_futures <- CreateConnection(user.name , machines ,"VALIDATION_NSE_ONLC_futures")
  connection.VALIDATION_TWS_OHLC_stocks <- CreateConnection(user.name , machines ,"VALIDATION_TWS_OHLC_stocks")
  connection.VALIDATION_TWS_PERMIN_futures <- CreateConnection(user.name , machines ,"VALIDATION_TWS_PERMIN_futures")
  connection.VALIDATION_TWS_PERMIN_stocks <- CreateConnection(user.name , machines ,"VALIDATION_TWS_PERMIN_stocks")
  
  
  

###################################   NSE  ############################################
  query1<-"SELECT MIN(END_DATE),VERSION FROM NSE_OHLC_futures"
  result <- dbGetQuery(connection.METADATA_DATABASE,query1)
  min.end.date.nse.fut <- result[1,1]
  version <- as.numeric(result[1,2])
  
  downloadUnzipAndAddNseFiles( a = as.character(min.end.date.nse.fut),user.name= user.name,connection = connection.temp,version = 0 ,)
  Filter(user.name,connection=connection.temp)
  
  querydate1<-paste("UPDATE future SET TIMESTAMP=STR_TO_DATE(TIMESTAMP,'%d-%M-%Y')")
  dbGetQuery(connection.temp,querydate1)

  querydate2<-paste("UPDATE future SET EXPIRY_DT=STR_TO_DATE(EXPIRY_DT,'%d-%M-%Y')")
  dbGetQuery(connection.temp,querydate2)

  querydate3<-paste("UPDATE equity SET TIMESTAMP=STR_TO_DATE(TIMESTAMP,'%d-%M-%Y')")
  dbGetQuery(connection.temp,querydate3)
  
  
  SplitFutureIntoTables(connection.temp,connection.NSE_OHLC_futures)
  SplitEquityIntoTables(connection.temp,connection.NSE_OHLC_stocks)
  
###################################   TWS OHLC  ############################################  
  
  query2<-"SELECT MIN(END_DATE),VERSION FROM TWS_OHLC_stocks"
  result <-  as.character(dbGetQuery(connection.METADATA_DATABASE,query2))
    min.end.date.tws.ohlc.stk <- result[1,1]
  version <- as.numeric(result[1,2])
  UpdateTwsOhlc(connection.TWS_OHLC_stocks,min.end.date.tws.ohlc.stk,version)
  
  
###################################   TWS PERMIN STOCKS  ############################################  
  for(i in 1 : no ){

  tablename= as.character(futures_list[i,4])
  query3 <- paste("SELECT MIN(END_DATE),VERSION FROM TWS_PERMIN_stocks WHERE TABLE_NAME = '", tablename , "'" , sep="")
  result  <- as.character(dbGetQuery(connection.METADATA_DATABASE,query3))
  min.end.date.tws.permin.stk <- result[1,1]
  version <- as.numeric(result[1,2])
  
  
  if(nchar(min.end.date.tws.permin.stk) > 2) 
   {
    UpdatePerminStocks(connection = connection.TWS_PERMIN_stocks , tablename , start.date = min.end.date.tws.permin.stk , version = version)
  }
  } 
###################################   TWS PERMIN FUTURES  ############################################  
  
  for(i in 1 : no ){
  tablename= as.character(futures_list[i,4])
  query4 <- paste("SELECT MIN(END_DATE),VERSION FROM TWS_PERMIN_futures WHERE TABLE_NAME = '", tablename , "'" , sep="")
  result <- as.character(dbGetQuery(connection.METADATA_DATABASE,query4))
  min.end.date.tws.permin.fut <- result[1,1]
  version <- as.numeric(result[1,2])
  if(nchar(min.end.date.tws.permin.fut) > 2) 
   {
    UpdatePerminFutures(connection = connection.TWS_PERMIN_futures , tablename , start.date = min.end.date.tws.permin.fut , version = version)
  }

  }
#################################  Updating done #####################################################

 dbRemoveTable(conection.METADATA_DATABASE)

 CreateMetadataDatabase(connection.METADATA_DATABASE ,
                        connection.NSE_OHLC_stocks,
                        connection.NSE_ONLC_futures,
                        connection.TWS_OHLC_stocks,
                        connection.TWS_PERMIN_futures,
                        connection.TWS_PERMIN_stocks
                        )
  
  RemoveTables(connection.VALIDATION_NSE_OHLC_stocks)
  RemoveTables(connection.VALIDATION_NSE_ONLC_futures)
  RemoveTables(connection.VALIDATION_TWS_OHLC_stocks)
  RemoveTables(connection.VALIDATION_TWS_PERMIN_futures)
  RemoveTables(connection.VALIDATION_TWS_PERMIN_stocks)
  
  NseOhlcFuturesValidateForTime(user.name)
  NseOhlcStocksValidateForTime(user.name)
  TwsOhlcStocksValidateForTime(user.name)
  TwsPerminFuturesValidateForTime(user.name)
  TwsPerminStocksValidateForTime(user.name)
  
}