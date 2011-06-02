library(IBrokers)
#tws<-twsConnect(clientId=2)

stk<-read.csv("STK.with.Sym.csv", sep=",")
tryCatch(for(i in 2:223){
  
  Sys.sleep(8)
  s<-stk[i,3]
  s<-as.character(s)
  t<-twsSTK(s, exch ="NSE" ,currency="INR")
  reqHistoricalData(tws,t,file=paste(as.character(stk[i,2]),".per.day.data.csv"),duration="1 Y",barSize="1 day",verbose = TRUE,)
} ,
error=function(e) {
  print(" error ")
 } )