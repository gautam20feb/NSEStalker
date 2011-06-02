library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
returns<-function(
a,
### a takes starting date as input. FORMAT: "yy-mm-dd"
b,
### b takes end date as input
s
### s takes vector of stocks symbols. ex., c("ABB","ACC")
)
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
### Marking all the Weekends as holidays
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
nstks<-length(s)
days<-length(tS)
temp<-0          ## to keep the counter of number of working days
strdates=""      ## string of dates that will be called in sql query
for(i in 1:days){
  if(trading[i]=="Working Day"||settlement[i]=="Working Day"){
    temp<-temp+1        ### + 1 working day
    if(as.numeric(date[i])<10 && as.numeric(year[i])>2010 ) ## to handle the case of year 2011 with dates as 01,02...
      singledate<-paste("0",date[i],"-",mont[i],"-",year[i],sep="")
    else
      singledate<-paste(date[i],mont[i],year[i],sep="-")
    strdates<-paste(strdates,",'",singledate,"'",sep="")
  }
}
strdates<-substr(strdates,2,nchar(strdates)) ## ignores initial comma
### string of stock name that will be called in sql query
strStk=""
for(i in 1:l){
  strStk<-paste(strStk,",'",s[i],"'",sep="")
}
strStk<-substr(strStk,2,nchar(strStk))
### creates a sql query using strStk and strdates
query<-paste("SELECT CLOSE,PREVCLOSE,TIMESTAMP,SYMBOL FROM equity WHERE TIMESTAMP IN ","(",strdates,")"," AND SYMBOL IN","(",strStk,")"," AND SERIES ='EQ' ORDER BY SYMBOL",sep="")
table<-dbGetQuery(con,query) ## implements the query
ret<-table[1]-table[2]       ## calculates return in a vector 
pvalue<-seq(nstks)           ## will hold the p values of each stock returns
show_pvalue<-seq(nstks)      ## will hold "pvalue = x" where x is rounded p value of each stock with 3 decimal digits 
## calculate the p value of each stock
for(i in 1:length(nstks)){  
  test<-shapiro.test(ret[(1+((i-1)*(temp))):(i*temp),1]) ## return values are stored in column order by each stock
  pvalue[i]<-test[[2]]
  show_pvalue[i]<-paste("pvalue=",round(pvalue[i],3))
}
par(bg="gray")          ## sets the backgroud color of graph as gray
col<-heat.colors(nstks) ## creates a vector containing unique colors for each stock 
### plots the graph with 1st stock returns and labels, limits for x and y axis
plot(ret[1:temp,1],type="l",col=col[1],xlim=c(1,temp),ylim=c(min(ret),max(ret)),main="Return in given stocks per day",xlab="days",
    ylab="Return (Rupees)",lwd=2)  
### plots returns for each stock with different colours
for(i in 2:l){   
  lines(ret[(1+((i-1)*(temp))):(i*temp),1],type="l",col=col[i],lwd=2)
  }  
leg<-paste(s,show_pvalue,sep=" , ") ## creates a vector containing stock name and p value. Ex, ABB , pvalue=0.3
legend("topright",legend=leg,col=col,lty=1,lwd=2) ## puts the legend at topright position with values in leg vector 
}