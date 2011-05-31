
library(fBasics)
library(fImport)
routines<-function(a,b,s)
{
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
for(i in 1:t){
  if(trading[i]=="Working Day"||settlement[i]=="Working Day"){
    temp<-temp+1
    if(as.integer(date[i])<10){
      temp1<-as.character(composeURL("cm0",date[i],mont[i],year[i],"bhav.csv"))
      equity[i]<-substr(temp1,8,26)
    }
    else{
      temp1<-as.character(composeURL("cm",date[i],mont[i],year[i],"bhav.csv"))
      equity[i]<-substr(temp1,8,26)
    }
    db<-read.csv(equity[i])
    for(g in 1:l){
      cs=which(db[,1]==s[g])
      mat[g,i]=db[cs,6]-db[cs,8]
    }
  }
  else{
    for(g in 1:l){
      mat[g,i]=50000
    }
  }
}  
D <- matrix(seq(temp*l)-seq(temp*l), nrow = l, ncol=temp, byrow=TRUE)
k<-1
j<-1
while((k<(t+1))||j<(temp+1)){
  if(mat[1,k]!=50000){
    D[,j]=mat[,k]
    j<-j+1
  }
  k<-k+1
}
plot(D[1,],type="l",col="red",ylim=c(min(D),max(D)),main="Return in given stocks per day",xlab="days",
    ylab="Return (dollar)",lwd=2)
lines(D[2,],type="l",col="blue",lwd=2)
lines(D[3,],type="l",col="green",lwd=2)
lines(D[4,],type="l",col="orange",lwd=2)
D
legend("topright",
legend=s,
col=c("red","blue","green","orange"),
lty=1,lwd=2)

}