library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
m <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE derivedDB")
con1 <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE rawdatabase")
ser <-function(start,end)
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
  stk<-as.character(stock[,1])
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
  l2<-seq(length(table1[,1])-length(fs))
  out <- apply(expand.grid(l1,l2), MARGIN = 1, function(x) if(x[1]>x[2]) c(x[1],x[2]) else c(NA,NA))
  out<-t(na.omit(t(out)))
  out<-as.data.frame(out)
  k<-1
  assign("k", k, envir=globalenv())
  sapply(out,cal)
  
}

cal<-function(a)
{
    a<-as.matrix(a)
    coeffk<<-cor.test(ret1[,a[1]],ret1[,a[2]],method="kendall") ## calculates kendall coeffecients
    coeffp<<-cor.test(ret1[,a[1]],ret1[,a[2]])
    coeffs<<-cor.test(ret1[,a[1]],ret1[,a[2]],method="spearman")
    coeffk1<<-cor.test(cls1[,a[1]],cls1[,a[2]],method="kendall") ## calculates kendall coeffecients
    coeffp1<<-cor.test(cls1[,a[1]],cls1[,a[2]]) 
    coeffs1<<-cor.test(cls1[,a[1]],cls1[,a[2]],method="spearman")
    print(c(a[1],a[2]))
    print(c((stk[a[1]]),(stk[a[2]])))
    tobe<<-c((stk[a[1]]),(stk[a[2]]),round(coeffk[[4]],3),round(coeffp[[4]],3),round(coeffs[[4]],3),round(coeffk1[[4]],3),round(coeffp1[[4]],3),round(coeffs1[[4]],3))    
    d<-data.frame()
    r<-rbind(d,tobe)
    names(r)<-c("stock1","stock2","kend_ret","pears_ret","spear_ret","kend_price","pears_price","spear_price")
    row.names(r)<-k
    k<<-k+1
#     print(r)
    dbWriteTable(con, name ="futures_correlation", value=r,append=T)
}