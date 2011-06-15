library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
m <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE derivedDB")
con1 <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE rawdatabase")
pval<-function(start,end)
{
 query1<-paste("SELECT DISTINCT SYMBOL,COUNT(*) as OCCURENCE FROM future WHERE TIMESTAMP BETWEEN '",start,"' AND '",end,"' GROUP BY SYMBOL",sep="") 
  query<-paste("SELECT * FROM fut_return WHERE TIMESTAMP BETWEEN '",start,"' AND '",end,"'",sep="")
  table1<-dbGetQuery(con1,query1)
  tab<-dbGetQuery(con,query)
  stock<-as.data.frame(table1[,1])
  colnames(stock)<-c("stock")
  faulty<-which(table1[,2] != max(table1[,2]))
  fs<-table1[faulty,1]
  sapply(fs,function(x) tab<<-tab[(tab$SYMBOL)!= x,] )
  sapply(fs,function(x) stock<<-as.data.frame(stock[(stock$stock)!= x,]) )
  stock<-as.character(stock[,1])
  assign("stk", stock, envir=globalenv())
  cls<-tab[5]
  ret<-tab[6]
  ret<-as.data.frame(ret)
  cls<-as.data.frame(cls)
  ret<-as.matrix(ret)
  cls<-as.matrix(cls)
  ret<-as.numeric(ret)
  cls<-as.numeric(cls)
  dim(ret)<-c((max(table1[,2])/3),(length(table1[,1])-length(fs)))
  dim(cls)<-c((max(table1[,2])/3),(length(table1[,1])-length(fs)))
  assign("ret1", ret, envir=globalenv())
  assign("cls1", cls, envir=globalenv())
  l1<-seq(length(table1[,1])-length(fs))
   k<-1
  assign("k", k, envir=globalenv())
  sapply(l1,cal)
#   return(ret)
}
cal<-function(a)
{
#     a<-as.matrix(a)
    test<-shapiro.test(ret1[,a])
    print(a)
    print((stk[a]))
    tobe<<-c((stk[a]),round(test[[2]],4))    
    d<-data.frame()
    r<-rbind(d,tobe)
    names(r)<-c("stock","p_value")
    row.names(r)<-k
    k<<-k+1
#     print(r)
    dbWriteTable(con, name ="futures_pvalues", value=r,append=T)
}
