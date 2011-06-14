setwd("~/projects/NSEStalker/validation")

library(RMySQL)
m<- dbDriver("MySQL")
### Connection to TWS Database
conn<-dbConnect(m, dbname = "TWS_daily_data", user = "root", password = "intern123", host = "localhost")
### Connection to NSEDB
conn1<-dbConnect(m, dbname = "nsedb", user = "root", password = "intern123", host = "localhost")

### File connections for logging "Uncommon Dates" and "Tracking Status"
datelog<-file("Diff_date_log.csv","w")
mylog2 <- file("Stk_OHLC_Validation_log.csv", "w")  ##<< For logging the files written to database
error<-file("Error_validation_log.csv","w")

### Initialization of vectors
open_diff<-c()
high_diff<-c()
low_diff<-c()
close_diff<-c()
comp<-c()
common_dates<-c()
diff<-c()

### List of Companies(from TWS databse)
nse_companies<-dbGetQuery(conn,"SELECT `NSE_NAME` FROM Stocks")
tws_companies<-dbGetQuery(conn,"SELECT `NAME_USED` FROM Stocks")
### List of Companies(from NSE databse)
nsedb_companies<-dbGetQuery(conn1,"SELECT DISTINCT `SYMBOL`FROM equity")
### List of common companies from two databases
common_companies<-intersect(nse_companies[,1],nsedb_companies[,1])

### Check for companies mismatch
if (nrow(nse_companies) != length(common_companies)) print("Error : List of Companies from IB is not the same as List of comapnies from NSEDB")

 for(j in 1:length(common_companies))
 {
   ### Vector of dates for which data is available for a given company from TWS databse
   query<-paste("SELECT TIMESTAMP FROM `",tws_companies[j,1],"`",sep="")
   date_TWS<-dbGetQuery(conn,query)
   ### Vector of dates for which data is available for a given company from NSE databse
   query<-paste("SELECT `TIMESTAMP` FROM `equity`WHERE `SYMBOL` = '",nse_companies[j,1],"'",sep="")
   date_nsedb<-dbGetQuery(conn1,query)
   ### Intersection of two date vectors
   common_dates<-intersect(date_TWS[,1],date_nsedb[,1])
   diff<-setdiff(date_TWS[,1],date_nsedb[,1])
   ### Logging for Dates Mismatch in the two databses
   for(k in 1: length(diff))
   {
     if(length(which(date_TWS==diff[k]))==0) remark<-"Present in NSEDB but not present in TWS"
     if(length(which(date_nsedb==diff[k]))==0) remark<-"Present in TWS but not present in NSEDB"
     cat(as.character(j),"Company:",common_companies[j],"Date:",diff[k],"Remark:",remark,"\n",file = datelog, sep = ",")
   }
   
   for(i in 1:length(common_dates))
   {
     ### OHLC data from NSEDB
     query<-paste("SELECT * FROM `equity`WHERE `TIMESTAMP` = '",common_dates[i],"' AND `SYMBOL` = '",nse_companies[j,1],"'",sep="")
     t1<-system.time(open_nsedb<-dbGetQuery(conn1,query))
     
     ### OHLC data from TWSDB
     query<-paste("SELECT * FROM `",tws_companies[j,1],"` WHERE `TIMESTAMP`='",common_dates[i],"'",sep="")
     t2<-system.time(open_TWS<-dbGetQuery(conn,query))
     ### Differnce of two values (Validation)
     open_diff[i]<-open_nsedb[1,4]-open_TWS[1,3]
     high_diff[i]<-open_nsedb[1,5]-open_TWS[1,4]
     low_diff[i]<-open_nsedb[1,6]-open_TWS[1,5]
     close_diff[i]<-open_nsedb[1,7]-open_TWS[1,6]
     comp[i]<-common_companies[j]
     
     ### Logging cited errors during validation
     if(open_diff[i] > 2|| high_diff > 2 || low_diff > 2 || close_diff > 2)
     {
       cat(as.character(j),"Company:",common_companies[j],"Date:",common_dates[i],"Open_diff=",open_diff[i],"High_diff=",high_diff[i],"Low_diff=",low_diff[i],"Close_diff=",close_diff[i],"\n",file = error, sep = ",")
     }
     
     ### Logging for tracking status
     cat(as.character(timestamp()),"Date",common_dates[i],"Stock",as.character(j),common_companies[j],"CPU Time for SQL Query","T1=(",t1,")  T2=(",t2,")","\n",file = mylog2, sep = ",")
     
   }
   ### Writing output of validation to csv
   all<-cbind(comp,common_dates,open_diff,high_diff,low_diff,close_diff)
   write.table(all, file="validation.csv",col.names=NA, append=TRUE, sep=",")
}

dbDisconnect(conn)
dbDisconnect(conn1)