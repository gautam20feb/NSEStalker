library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
m <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE derivedDB")
con1 <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE rawdatabase")
st <-function(start,end)
{
  query<-paste("SELECT INSTRUMENT, SYMBOL,  min(EXPIRY_DT),OPEN,HIGH,LOW,CLOSE,SETTLE_PR,CONTRACTS,VAL_INLAKH, OPEN_INT,CHG_IN_OI, TIMESTAMP FROM future WHERE TIMESTAMP BETWEEN '",start,"' AND '",end,"' GROUP BY TIMESTAMP,SYMBOL ORDER BY SYMBOL,TIMESTAMP",sep="")
  tab<-dbGetQuery(con1,query)
  dbWriteTable(con, name = "future_earliest_expiry_dt", value=tab,append=T)
}