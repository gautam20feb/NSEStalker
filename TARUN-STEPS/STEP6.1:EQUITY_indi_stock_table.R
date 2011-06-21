library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
m <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE_raw_database")
con1 <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE_OHLC_stocks")
stkt <-function()
{
  
  query1<-paste("SELECT DISTINCT SYMBOL FROM equity ",sep="")
  tables<-dbGetQuery(con,query1)
  tables<-tables[,1]
  print("got the tables")
  assign("tables", tables, envir=globalenv())
  sapply(tables,cal)
}
cal<-function(x)
{ 
  #x=tables[5]
  query<-paste("SELECT * FROM equity WHERE SYMBOL = '" , x,"' ORDER BY SYMBOL,TIMESTAMP",sep="")
  stk1<-dbGetQuery(con,query)
  stk1$row_names<-NULL
 # stk1<-tab[(tab$SYMBOL)==x,]
  row.names(stk1)<-seq(length(stk1[,1]))
  if(x=="BAJAJ-AUTO") x="BAJAJAUTO"
  if(x=="M&M")x="MANDM"
  if(x=="MCDOWELL-N")x="MCDOWELLN"
  if(x=="TV-18")x="TV18"
  if(x=="AREVAT&D")x="AREVATD"
  if(x=="IT&T") x= "ITANDT"
  if(x=="L&T")x="LANDT"
  if(x=="J&KBANK")x = "JANDKBANK"
  if(x=="HCL_INSYS") x= "HCLINSYS"
  print(paste("uploading ",x))
  dbWriteTable(con1, name = x, value=stk1,append=T)
}