library(timeDate)
library(fBasics)
library(fImport)
tS = timeSequence(from = "2001-01-01", to = "2011-12-31", by = "day")
char<-as.character(tS)
day<-dayOfWeek(tS)
trading<-as.character(seq(length(char)))
settlement<-as.character(seq(length(char)))
reason<-as.character(seq(length(char)))
mont<-as.character(seq(length(char)))
year<-as.character(seq(length(char)))
date<-as.character(seq(length(char)))
equity<-as.character(seq(length(char)))
derivative<-as.character(seq(length(char)))
wdm<-as.character(seq(length(char)))
debt<-as.character(seq(length(char)))
rdm<-as.character(seq(length(char)))
slbs<-as.character(seq(length(char)))

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
  if(day[i]=="Sat" || day[i]=="Sun")
  {
    trading[i]<-"Holiday"
    settlement[i]<-"Holiday"
    reason[i]<-"Weekend"
  }
  else{
    trading[i]<-"Working Day"
    settlement[i]<-"Working Day"
    reason[i]<-""
  }
  for(j in 1:nrow(holiday)){
    if(as.character(char[i]) == as.character(holiday[j,1]))
    {
      trading[i]<-"Holiday"
      settlement[i]<-"Holiday"
      reason[i]<-as.character(holiday[j,3])
    }
  }
}
################################## URL #########################################################
for(i in 1:length(tS)){
  if(trading[i]=="Working Day"||settlement[i]=="Working Day")
  {
    equity[i]<-as.character(composeURL("www.nseindia.com/content/historical/EQUITIES/",year[i],"/",mont[i],"/cm",date[i],mont[i],year[i],"bhav.csv.zip"))
    derivative[i]<-as.character(composeURL("www.nseindia.com/content/historical/DERIVATIVES/",year[i],"/",mont[i],"/fo",date[i],mont[i],year[i],"bhav.csv.zip"))
    wdm[i]<-as.character(composeURL("www.nseindia.com/content/historical/WDM/",year[i],"/",mont[i],"/wdmlist_",date[i],atom[i,2],year[i],".csv"))
    debt<-as.character(composeURL("www.nseindia.com/archives/debt/cbm/cbm_trd",year[i],atom[i,2],date[i],".csv"))
    rdm<-as.character(composeURL("www.nseindia.com/content/historical/RDM/",year[i],"/",mont[i],"/rdm",date[i],mont[i],year[i],"bhav.csv.zip"))
    slbs<-as.character(composeURL("www.nseindia.com/archives/slbs/bhavcopy/SLBM_BC_",date[i],atom[i,2],year[i],".DAT"))
  }
  else{
    equity[i]<-""
    derivative[i]<-""
    }
}

################################## Printing ###################################################

all<-cbind(char,day,trading,settlement,reason,date,mont,year,equity,derivative,wdm,debt,rdm,slbs)
file <- tempfile()
x <- matrix(all, nrow =length(char),ncol=14, dimnames = list(c(), c("Date", "Day","For Trading", "For Settlement", "Reason", "Date", "Month","Year","Equity URL","Derivative URL","WDM URL","DEBT URL","RDM URL","SLBS URL")))
write.csv(x, file)
read.csv(file)