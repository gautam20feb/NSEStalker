library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
m <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE derivedDB")
top<-function()
{
  query<-paste("SELECT * FROM futures_correlation",sep="")
  tab<-dbGetQuery(con,query)
  sort1 <- tab[order(tab$kend_ret,decreasing=TRUE) , ]
  sort2 <- tab[order(tab$pears_ret,decreasing=TRUE) , ]
  sort3 <- tab[order(tab$spear_ret,decreasing=TRUE) , ]
  sort4 <- tab[order(tab$kend_price,decreasing=TRUE) , ]
  sort5 <- tab[order(tab$pears_price,decreasing=TRUE) , ]
  sort6 <- tab[order(tab$spear_price,decreasing=TRUE) , ]
  plot_cor(sort1,sort2,sort3,sort4,sort5,sort6)
  topall<-topofall(sort1,sort2,sort3,sort4,sort5,sort6)
  return(topall)

}
plot_cor<-function(sort1,sort2,sort3,sort4,sort5,sort6)
{
  layout(mat=matrix(1:6, 2, 3, byrow=TRUE))
  kend_of_return<-sort1[,4]
  kend_of_return<-as.matrix(kend_of_return)
  kend_of_return<-as.numeric(kend_of_return)
  hist(kend_of_return,breaks<-40)
  pearson_of_return<-sort2[,5]
  pearson_of_return<-as.matrix(pearson_of_return)
  pearson_of_return<-as.numeric(pearson_of_return)
  hist(pearson_of_return,breaks<-40)
  spearman_of_return<-sort3[,6]
  spearman_of_return<-as.matrix(spearman_of_return)
  spearman_of_return<-as.numeric(spearman_of_return)
  hist(spearman_of_return,breaks<-40)
  
  kend_of_price<-sort4[,7]
  kend_of_price<-as.matrix(kend_of_price)
  kend_of_price<-as.numeric(kend_of_price)
  hist(kend_of_price,breaks<-40)
  pearson_of_price<-sort5[,8]
  pearson_of_price<-as.matrix(pearson_of_price)
  pearson_of_price<-as.numeric(pearson_of_price)
  hist(pearson_of_price,breaks<-40)
  spearman_of_price<-sort6[,9]
  spearman_of_price<-as.matrix(spearman_of_price)
  spearman_of_price<-as.numeric(spearman_of_price)
  hist(spearman_of_price,breaks<-40)
}
topofall<-function(sort1,sort2,sort3,sort4,sort5,sort6)
{
  top1<-sort1[1:50,2:4]
  top2<-sort2[1:50,c(2,3,5)]
  top3<-sort3[1:50,c(2,3,6)]
  top4<-sort4[1:50,c(2,3,7)]
  top5<-sort5[1:50,c(2,3,8)]
  top6<-sort6[1:50,c(2,3,9)]
  top_all<-cbind(top1,top2,top3,top4,top5,top6)
  row.names(top_all)<-seq(50)
  return(top_all)
}