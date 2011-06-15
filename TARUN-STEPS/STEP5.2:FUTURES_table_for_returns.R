library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
fut<-function(
start,
end
)
{
n <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(n, user="root", password = "intern123", host = "localhost", dbname="NSE derivedDB")
query<-paste("SELECT * FROM future_earliest_expiry_dt WHERE TIMESTAMP  BETWEEN '",start,"' AND '",end,"'",sep="")
tab<-dbGetQuery(con,query) ## implements the query
retn<-seq(length(tab[,1]))
k<-1
l<-seq(length(tab[,1]))
l1<-l[2:length(l)]
check<-c(FALSE,tab[l1,3]==tab[l,3])
check<-check[-length(check)]
check2<-c(FALSE,tab[l,8]!=0)
check2<-check2[-length(check2)]
check3<-(check&check2)
returns<-c(0,log(tab[l1,8]/tab[l,8]))
returns<-returns[-length(returns)]
returns<-ifelse(check3,returns,NA)
r<-cbind(tab[,3],tab[,14],tab[,4],tab[,8],round(returns,3))
r<-as.data.frame(r)
names(r)<-c("SYMBOL","TIMESTAMP","earliest EXPIRY_DT","CLOSE","RET")
dbWriteTable(con, name ="fut_return", value=r,append=T)
}
