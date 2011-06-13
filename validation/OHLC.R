library(RMySQL)
m<- dbDriver("MySQL")
conn<-dbConnect(m, dbname = "TWS_daily_data", user = "root", password = "intern123", host = "localhost")
conn1<-dbConnect(m, dbname = "nsedb", user = "root", password = "intern123", host = "localhost")

mylog2 <- file("Validation_log.csv", "w")  ##<< For logging the files written to database

open_diff<-c()
high_diff<-c()
low_diff<-c()
close_diff<-c()
comp<-c()

companies<-dbListTables(conn)

for(j in 1:length(companies))
{
query<-paste("SELECT TIMESTAMP FROM `",companies[j],"`",sep="")
date_TWS<-dbGetQuery(conn,query)

query<-paste("SELECT `TIMESTAMP` FROM `equity`WHERE `SYMBOL` = '",companies[j],"'",sep="")
date_nsedb<-dbGetQuery(conn1,query)

common_dates<-intersect(date_TWS[,1],date_nsedb[,1])
diff<-setdiff(date_TWS[,1],date_nsedb[,1])

for(i in 1:length(common_dates))
{
  query<-paste("SELECT * FROM `equity`WHERE `TIMESTAMP` = '",common_dates[i],"' AND `SYMBOL` = '",companies[j],"'",sep="")
  open_nsedb<-dbGetQuery(conn1,query)
  
  query<-paste("SELECT * FROM `",companies[j],"` WHERE `TIMESTAMP`='",common_dates[i],"'",sep="")
  open_TWS<-dbGetQuery(conn,query)
  
  open_diff[i]<-open_nsedb[1,4]-open_TWS[1,3]
  high_diff[i]<-open_nsedb[1,5]-open_TWS[1,4]
  low_diff[i]<-open_nsedb[1,6]-open_TWS[1,5]
  close_diff[i]<-open_nsedb[1,7]-open_TWS[1,6]
  comp[i]<-companies[j]
  
  cat(as.character(timestamp()),"Date",common_dates[i],"Stock",as.character(j),companies[j],"\n",file = mylog2, sep = ",")

  
}

all<-cbind(comp,common_dates,open_diff,high_diff,low_diff,close_diff)
write.table(all, file="validation.csv", append=TRUE, sep=",")
}

dbDisconnect(conn)
dbDisconnect(conn1)