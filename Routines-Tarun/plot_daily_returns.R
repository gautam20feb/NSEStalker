library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
routines<-function(a,b,s)
{
m <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="nsedb")
holiday<-read.csv("holiday.csv")
tS = timeSequence(from = a, to = b, by = "day")
char<-as.character(tS)
day<-dayOfWeek(tS)
trading<-as.character(seq(length(char)))   ##<<  The trading days
settlement<-as.character(seq(length(char))) ##<<  The settlement days
reason<-as.character(seq(length(char)))    ##<<  Reason for being a holiday
mont<-as.character(seq(length(char)))      ##<< Month
year<-as.character(seq(length(char)))       
date<-as.character(seq(length(char)))
equity<-as.character(seq(length(char)))
atom<-atoms(tS)
for (i in 1:length(tS)){
  if(atom[i,2]==1) mont[i]<-"JAN"
  if(atom[i,2]==2) mont[i]<-"FEB"
  if(atom[i,2]==3) mont[i]<-"MAR"
  if(atom[i,2]==4) mont[i]<-"APR"
  if(atom[i,2]==5) mont[i]<-"MAY"
  if(atom[i,2]==6) mont[i]<-"JUN"
  if(atom[i,2]==7) mont[i]<-"JUL"
  if(atom[i,2]==8) mont[i]<-"AUG"
  if(atom[i,2]==9) mont[i]<-"SEP"
  if(atom[i,2]==10) mont[i]<-"OCT"
  if(atom[i,2]==11) mont[i]<-"NOV"
  if(atom[i,2]==12) mont[i]<-"DEC"
  year[i]<-atom[i,1]
  date[i]<-atom[i,3]
}
for(i in 1:length(char)){
  if(day[i]=="Sat" || day[i]=="Sun"){
    trading[i]<-"Holiday"
    settlement[i]<-"Holiday"
    reason[i]<-"Weekend"
  }
  else{
    trading[i]<-"Working Day"
    settlement[i]<-"Working Day"
    reason[i]<-""
  }
  ### Marking all the holidays as as listed in holiday.csv
  for(j in 1:nrow(holiday)){
    if(as.character(char[i]) == as.character(holiday[j,1])){
      trading[i]<-"Holiday"
      settlement[i]<-"Holiday"  
      reason[i]<-as.character(holiday[j,3])
    }
  }
 }
l<-length(s)
t<-length(tS)
mat <- matrix(seq(t*l)-seq(t*l), nrow = l, ncol=t, byrow=TRUE)
temp<-0
W=""
for(i in 1:t){
  if(trading[i]=="Working Day"||settlement[i]=="Working Day"){
    temp<-temp+1
    w<-paste(date[i],mont[i],year[i],sep="-")
    W<-paste(W,",'",w,"'",sep="")
  }
}
W<-substr(W,2,nchar(W))

S=""
for(i in 1:l){
  S<-paste(S,",'",s[i],"'",sep="")
}

S<-substr(S,2,nchar(S))


query<-paste("SELECT CLOSE,PREVCLOSE,TIMESTAMP,SYMBOL FROM equity WHERE TIMESTAMP IN ","(",W,")"," AND SYMBOL IN","(",S,")","AND SERIES ='EQ' ORDER BY SYMBOL",sep="")
C<-dbGetQuery(con,query)
ret<-C[1]-C[2]
pvalue<-seq(length(s))
show_pvalue<-seq(length(s))
for(i in 1:length(s)){
  #print(ret[(1+((i-1)*(temp))):(i*temp),1])
  test<-shapiro.test(ret[(1+((i-1)*(temp))):(i*temp),1])
  pvalue[i]<-test[[2]]
  show_pvalue[i]<-paste("pvalue=",round(pvalue[i],3))
}
par(bg="gray")
col<-heat.colors(l)
plot(ret[1:temp,1],type="l",col=col[1],xlim=c(1,temp),ylim=c(min(ret),max(ret)),main="Return in given stocks per day",xlab="days",
    ylab="Return (Rupees)",lwd=2)

for(i in 2:l){   
  lines(ret[(1+((i-1)*(temp))):(i*temp),1],type="l",col=col[i],lwd=2)
  }  
leg<-paste(s,show_pvalue,sep=" , ")
legend("topright",legend=leg,col=col,lty=1,lwd=2)
 }