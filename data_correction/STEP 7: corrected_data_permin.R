setwd("~/correction")
library(timeSeries)
library(xts)
library(RMySQL)
library(gregmisc)
library(foreach)
library(doMC)
library(multicore)
registerDoMC(cores=7)

bonus <- read.csv("bonus2.csv")
splits <- read.csv("splits2.csv")
TIMELINE_futures <- read.csv("TIMELINE_futures.csv")
nearest_expiry_date <- read.csv("nearest_expiry_date.csv")
futures_list <- trim(read.csv("futures_list.csv", sep=","))

# tables <- trim(as.character(futures_list$TABLE_NAME))

##############################################################################################################


######################################################################################################

m <- dbDriver("MySQL")
conn <- dbConnect(m, dbname = "TWS_PERMIN_futures", user = "intern", password = "intern123", host = "192.168.1.106")
tables <- dbListTables(conn)
dbDisconnect(conn)
for(co in dbListConnections(m)) dbDisconnect(co)
dbUnloadDriver(m)

mylog <- file("log_PERMIN.csv", "w")

foreach(i = 1:220)%dopar%
{
  m <- dbDriver("MySQL")
  conn <- dbConnect(m, dbname = "TWS_PERMIN_futures", user = "intern", password = "intern123", host = "192.168.1.106")
  conn1<-dbConnect(m, dbname = "CORRECTED_PERMIN_futures", user = "intern", password = "intern123", host = "192.168.1.106")
  conn2 <- dbConnect(m, dbname = "ADJUSTMENT_FACTORS", user = "intern", password = "intern123", host = "192.168.1.106")

  query <- paste("SELECT * FROM ", tables[i] , " ORDER BY TIMESTAMP", sep = "")
  permin <- dbGetQuery(conn,query)
  
  split.date <- as.vector((trim(substr(permin$TIMESTAMP, 1,10))))
  
   query <- paste("SELECT * FROM ", tables[i] , " Order BY TIMESTAMP ",sep = "")
  adjusted <- dbGetQuery(conn2 , query)

  adj <- function(x) {
    ind <- which(adjusted$TIMESTAMP == as.character(x))
    return(adjusted[ind,])
  }
  t<-system.time( permin.adj <- sapply ( split.date, adj))
  
  mul <- as.numeric(permin.adj[3,])
  toadd <- as.numeric(permin.adj[4,])
  correct.close <-as.numeric(as.character(permin$CLOSE))*mul + toadd
  correct.high <- as.numeric(as.character(permin$HIGH))*mul + toadd
  correct.low <- as.numeric(as.character(permin$LOW))*mul + toadd
  correct.open <- as.numeric(as.character(permin$OPEN))*mul + toadd
  correct.wap <- as.numeric(as.character(permin$WAP))*mul + toadd
  
  permin2 <- cbind(permin$TIMESTAMP, 
                        permin$CLOSE  ,  
                       data.frame(mul) , 
                        toadd , 
                        correct.close , 
                        correct.high ,
                        correct.low , 
                        correct.open , 
                        permin$VOLUME ,
                        correct.wap)
    
  names(permin2)<-c("TIMESTAMP"
                   ,"OLD_CLOSE"
                   ,"ADJ_FACTOR_MUL"
                   ,"ADJ_FACTOR_MUL"
                   ,"CLOSE"
                   , "HIGH"
                   , "LOW"
                   ,"OPEN"
                   ,"VOLUME"
                   ,"WAP")
  permin2 <- data.frame(permin2)
  ifelse(dbExistsTable(conn1,tables[i] ),dbWriteTable(conn1,tables[i], value=permin2, append = T),dbWriteTable(conn1,tables[i], value=permin2))                
  
  cat(as.character(timestamp()),i,tables[i],"\n",file = mylog, sep = ",")
  
  dbDisconnect(conn)
  dbDisconnect(conn1)
  for(co in dbListConnections(m)) dbDisconnect(co)
  dbUnloadDriver(m)
}
close(mylog)
