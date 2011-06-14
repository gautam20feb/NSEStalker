library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
m <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="nsedb")
con1 <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="STOCKS")
stkt <-function()
{
  query<-paste("SELECT * FROM equity",sep="")
  query1<-paste("SELECT DISTINCT SYMBOL FROM equity",sep="") 
  tab<-dbGetQuery(con,query)
  t<-dbGetQuery(con,query1)
  delete<-c("row_names","row.names")
  tab <- tab[, !(names(tab) %in% delete)]
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
  dbWriteTable(con1, name = x, value=stk1,append=T)
}