setwd("~/Dropbox/Intern/Validation/")

library(RMySQL)
m<- dbDriver("MySQL")
conn<-dbConnect(m, dbname = "yahoo_data", user = "swetha", password = "intern123", host = "localhost")
conn1<-dbConnect(m, dbname = "nsedb", user = "swetha", password = "intern123", host = "localhost")

datelog<-file("Diff_date_log.csv","w")
mylog2 <- file("yahoo_nse_OHLC_validation_log.csv","w")  ##<< For logging the files written to database

open_diff<-c()
high_diff<-c()
low_diff<-c()
close_diff<-c()
company<-c()
common_dates<-c()
diff<-c()

yahoo_companies<-as.data.frame(dbListTables(conn))

nsedb_companies<-dbGetQuery(conn1,"SELECT DISTINCT `SYMBOL`FROM equityback")
common_companies<-as.data.frame(intersect(yahoo_companies[,1],nsedb_companies[,1]))

if (length(yahoo_companies) != length(common_companies)) print("Error : List of Companies from Yahoo is not the same as List of comapnies from NSEDB")
# 12,104,171    do not data for 2011
for(j in 1:length(common_companies))
 {
   query<-paste("SELECT `row_names` FROM `",yahoo_companies[j,1],"`",sep="")
   date_yahoo<-dbGetQuery(conn,query)
   
   query<-paste("SELECT `TIMESTAMP` FROM `equityback`WHERE `SYMBOL` = '",yahoo_companies[j,1],"`",sep="")
   date_nsedb<-dbGetQuery(conn1,query)
  
   common_dates<-intersect(date_yahoo[,1],date_nsedb[,1])
   diff<-setdiff(date_yahoo[,1],date_nsedb[,1])
   
   cat(as.character(j),common_companies[j],diff,"\n",file = datelog, sep = ",")
   
    query<-paste("SELECT * FROM `equityback`WHERE `SYMBOL` = '",yahoo_companies[j],"'",sep="")
     open_nsedb<-dbGetQuery(conn1,query)
     
     query<-paste("SELECT * FROM `",yahoo_companies[j],"`",sep="")
     open_yahoo<-dbGetQuery(conn,query)
   
   for(i in 1:length(common_dates))
   {
    x<-as.character(which(open_nsedb$TIMESTAMP==common_dates[i]))
    y<-as.character(which(open_yahoo$row_names==common_dates[i]))
     
     open_diff[i]<-open_nsedb[x,4]-open_yahoo[y,2]
     high_diff[i]<-open_nsedb[x,5]-open_yahoo[y,3]
     low_diff[i]<-open_nsedb[x,6]-open_yahoo[y,4]
     close_diff[i]<-open_nsedb[x,7]-open_yahoo[y,5]
     company[i]<-common_companies[j]
   }
   
     cat(as.character(timestamp()),"Date",common_dates[i],"Stock",as.character(j),common_companies[j],"\n",file = mylog2, sep = ",")
   
   all<-cbind(company,common_dates,open_diff,high_diff,low_diff,close_diff)
   write.table(all, file="validation.csv", col.names=NA,append=TRUE, sep=",")
}

dbDisconnect(conn)
dbDisconnect(conn1)