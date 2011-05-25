library(timeDate)
library(fBasics)
library(fImport)
library("RCurl")
library (bitops)
library (methods)
get.bhavcopy<-function(a,b)
{
  holiday<-read.csv("holiday.csv")
  tS = timeSequence(from = a, to = b, by = "day")
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
  for (i in 1:length(tS))
  {
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
  for(i in 1:length(char))
  {
    if(day[i]=="Sat" || day[i]=="Sun")
    {
      trading[i]<-"Holiday"
      settlement[i]<-"Holiday"
      reason[i]<-"Weekend"
    }
    else
    {
      trading[i]<-"Working Day"
      settlement[i]<-"Working Day"
      reason[i]<-""
    }
    for(j in 1:nrow(holiday))
    {
      if(as.character(char[i]) == as.character(holiday[j,1]))
      {
        trading[i]<-"Holiday"
        settlement[i]<-"Holiday"
        reason[i]<-as.character(holiday[j,3])
      }
    }
  }
  # creating URLs 
  for(i in 1:length(tS))
  {
    if(trading[i]=="Working Day"||settlement[i]=="Working Day")
    {
      if(as.integer(date[i])<10)
      {
        equity[i]<-as.character(composeURL("www.nseindia.com/content/historical/EQUITIES/",year[i],"/",mont[i],"/cm0",date[i],mont[i],year[i],"bhav.csv.zip"))
        derivative[i]<-as.character(composeURL("www.nseindia.com/content/historical/DERIVATIVES/",year[i],"/",mont[i],"/fo0",date[i],mont[i],year[i],"bhav.csv.zip"))
        wdm[i]<-as.character(composeURL("www.nseindia.com/content/historical/WDM/",year[i],"/",mont[i],"/wdmlist_0",date[i],atom[i,2],year[i],".csv"))
        debt<-as.character(composeURL("www.nseindia.com/archives/debt/cbm/cbm_trd",year[i],atom[i,2],"0",date[i],".csv"))
        rdm<-as.character(composeURL("www.nseindia.com/content/historical/RDM/",year[i],"/",mont[i],"/rdm0",date[i],mont[i],year[i],"bhav.csv.zip"))
        slbs<-as.character(composeURL("www.nseindia.com/archives/slbs/bhavcopy/SLBM_BC_0",date[i],atom[i,2],year[i],".DAT"))
      }
      else
      {
        equity[i]<-as.character(composeURL("www.nseindia.com/content/historical/EQUITIES/",year[i],"/",mont[i],"/cm",date[i],mont[i],year[i],"bhav.csv.zip"))
        derivative[i]<-as.character(composeURL("www.nseindia.com/content/historical/DERIVATIVES/",year[i],"/",mont[i],"/fo",date[i],mont[i],year[i],"bhav.csv.zip"))
        wdm[i]<-as.character(composeURL("www.nseindia.com/content/historical/WDM/",year[i],"/",mont[i],"/wdmlist_",date[i],atom[i,2],year[i],".csv"))
        debt<-as.character(composeURL("www.nseindia.com/archives/debt/cbm/cbm_trd",year[i],atom[i,2],date[i],".csv"))
        rdm<-as.character(composeURL("www.nseindia.com/content/historical/RDM/",year[i],"/",mont[i],"/rdm",date[i],mont[i],year[i],"bhav.csv.zip"))
        slbs<-as.character(composeURL("www.nseindia.com/archives/slbs/bhavcopy/SLBM_BC_",date[i],atom[i,2],year[i],".DAT"))
      }
    }
    else
    {
      equity[i]<-NA
      derivative[i]<-NA
    }
  }
  all<-cbind(char,day,trading,settlement,reason,date,mont,year,equity,derivative,wdm,debt,rdm,slbs)
  file <- tempfile()
  x <- matrix(all, nrow =length(char),ncol=14, dimnames = list(c(), c("Date", "Day","For Trading", "For Settlement", "Reason", "Date", "Month","Year","Equity URL","Derivative URL","WDM URL","DEBT URL","RDM URL","SLBS URL")))
  write.csv(x, file)
  read.csv(file)
  #write.csv(x,file="out.csv")
  # Downloading
  sapply(equity,downloadE)
  sapply(derivative,downloadD)
}

#####################################################
downloadD<-function(sURL)
{
  Sys.sleep(poisson())
  if(!is.na(sURL))
  {
    options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6;en-US; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12")
    tContent <- getURLContent(sURL, verbose = TRUE, useragent = getOption("HTTPUserAgent"))
    attributes(tContent) = NULL
    t<-nchar(sURL)
    c<-substr(sURL, t-19 ,t)
    c<-paste("DER",c,sep="")
    writeBin(tContent, c )    
  }
}

#####################################################
#####################################################
downloadE<-function(sURL)
{
  Sys.sleep(poisson())
  if(!is.na(sURL))
  {
    options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6;en-US; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12")
    tContent <- getURLContent(sURL, verbose = TRUE, useragent = getOption("HTTPUserAgent"))
    attributes(tContent) = NULL
    t<-nchar(sURL)
    c<-substr(sURL, t-19 ,t)
    c<-paste("EQ",c,sep="")
    writeBin(tContent, c )    
  }
}

#####################################################

poisson<- function()
{
  k = 0
  p = 1.0
  L <-exp(-4)
  while (p >= L)  
  {
    k<-k+1
    x1 <- runif(1, 0, 1)
    p <- p*x1
  }
  k<- k-1
  return (k)
}
#####################################################
#####################################################
