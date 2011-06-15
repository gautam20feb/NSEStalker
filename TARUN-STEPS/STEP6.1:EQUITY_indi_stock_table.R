library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
m <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE rawdatabase")
con1 <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="STOCKS_equity")
stkt <-function()
{
  query<-paste("SELECT SYMBOL,SERIES,OPEN,HIGH,LOW,CLOSE,LAST,PREVCLOSE,TOTTRDQTY,TOTTRDVAL,TIMESTAMP FROM equity WHERE SERIES ='EQ' ORDER BY SYMBOL,TIMESTAMP",sep="")
  query1<-paste("SELECT DISTINCT SYMBOL FROM equity ",sep="") 
  tab<-dbGetQuery(con,query)
  print("got the tab")
  t<-dbGetQuery(con,query1)
  print("got the t")
  assign("tab", tab, envir=globalenv())
  sapply(t[,1],cal)
}
cal<-function(x)
{ 
  stk1<-tab[(tab$SYMBOL)==x,]
  row.names(stk1)<-seq(length(stk1[,1]))
  if(x=="BAJAJ-AUTO") x="BAJAJAUTO"
  if(x=="M&M")x="MANDM"
  if(x=="MCDOWELL-N")x="MCDOWELLN"
  if(x=="TV-18")x="TV18"
  if(x=="AREVAT&D")x="AREVATD"
  print(paste("uploading ",x))
  dbWriteTable(con1, name = x, value=stk1,append=T)
}