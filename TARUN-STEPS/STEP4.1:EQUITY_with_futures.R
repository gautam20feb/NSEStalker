library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
m <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE derivedDB")
con1 <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE rawdatabase")
st <-function(start,end)
{
  stock<-read.csv("STK.csv")
  strStk=""
  sapply(stock[,2],function(x) strStk<<-paste(strStk,",'",x,"'",sep=""))
  strStk<-substr(strStk,2,nchar(strStk))
  query<-paste("SELECT SYMBOL,CLOSE,PREVCLOSE,TIMESTAMP FROM equity WHERE TIMESTAMP BETWEEN '",start,"' AND '",end,"' AND SYMBOL IN","(",strStk,")"," AND SERIES ='EQ' ORDER BY SYMBOL,TIMESTAMP",sep="")
  tab<-dbGetQuery(con1,query)
  dbWriteTable(con, name = "equity_with_futures", value=tab,append=T)
}