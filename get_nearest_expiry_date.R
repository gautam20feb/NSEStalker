library(timeDate)
library(fBasics)
library(fImport)
holiday<-read.csv("./data/holiday.csv", header = F)

### The function returns a matrix and also gives a file having the working days and corresponding nearest expiry date
get.nearest.expiry.date<-function(
start.date = "2001-01-01"
### The starting date
, 
end.date = "2011-12-31"
### The ending date
)
{
  
tS = timeSequence(from = start.date, to = end.date, by = "day") ##<< time sequence by day
tSm = timeSequence(from = start.date, to = end.date, by = "month")  ##<< time sequence by month
tStemp = timeSequence(from = start.date, to = timeLastDayInMonth(end.date), by = "day") ##<< temporary time sequence by day
char<-as.character(tS)
chartemp<-as.character(tStemp)
day<-dayOfWeek(tS)
daytemp<-dayOfWeek(tStemp)
holstemp<-as.character(seq(length(chartemp)))##<<  The holiday or working day  
no.working.days<-length(char) ##<< keep track of the number of working days
len.tS= no.working.days
### Marking all the Weekends as holidays
for(i in 1:length(chartemp))    
{
  if(daytemp[i]=="Sat" || daytemp[i]=="Sun")
  {
   holstemp[i]<-"Holiday"
   if(i<=len.tS)
    {
     no.working.days<- no.working.days -1
    }
  }
    else
    {  
     holstemp[i]<-"Working Day"
    }

#####################################################################################################################

### Marking all the holidays as as listed in holiday.csv
  for(j in 1:nrow(holiday))     
  {
   if(as.character(chartemp[i]) == as.character(holiday[j,1]))
    {
    holstemp[i]<-"Holiday"
    if(i<=len.tS)
      {
        no.working.days<- no.working.days -1
      }
    }
  }
}
  
all<-cbind(chartemp,daytemp,holstemp)
x <- matrix(all, nrow =length(chartemp),ncol=3, dimnames = list(c(), c("Date", "Day","For hols")))
############################### Last Working Day ###########################################
lwday <- timeLastNdayInMonth(tSm, nday = 4)  ##<< The last working days are initialised to Thursdays
z<-seq(length(lwday))
for(j in 1:length(lwday))
  {
      z[j]<-which(x==as.character(lwday[j]))  
  }

for(i in 1:length(lwday))
{
  while(holstemp[z[i]]=="Holiday")
  {
    lwday[i]<-as.Date(lwday[i]-1)
    z[i]<-(z[i]-1)
  }
}

########################################## For each Working day finding the expiry ###########################################################################

wday<-matrix(data=NA , nrow=no.working.days, ncol=2)  ##<< initialised list of all the working days and their nearest expiry date
k<-1
j<-1
for(i in 1:length(tS))
  {
    if(holstemp[i]=="Working Day" && j<= no.working.days)
    {
        wday[j,1]<-char[i]
  
	if(pmin(as.Date(char[i]),as.Date(lwday[k])) !=  as.Date(lwday[k]))

	{ 
	 wday[j,2] <- as.character(lwday[k])
	}
	else
	{
	wday[j,2] <- as.character(lwday[k])
	k<-k+1	
	
	}
	j<-j+1
    }
    
  }
wday1<- as.data.frame(wday)
names(wday1)<-c("Day","Nearest Expiry Day")
write.csv(wday1,file="./data/nearestexpirytime.csv")
return(wday)
}
