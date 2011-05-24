tS = timeSequence(from = "2010-04-01", to = Sys.Date(), by = "day")
char<-as.character(tS)
day<-dayOfWeek(tS)
trading<-as.character(seq(length(char)))
settlement<-as.character(seq(length(char)))
reason<-as.character(seq(length(char)))

for(i in 1:length(char)){
  if(day[i]=="Sat" || day[i]=="Sun")
  {
    trading[i]<-"Hol"
    settlement[i]<-"Hol"
    reason[i]<-"Weekend"
  }
  else{
    trading[i]<-"Working"
    settlement[i]<-"Working"
    reason[i]<-"-"
  }
  for(j in 1:nrow(holiday)){
    if(as.character(char[i]) == as.character(holiday[j,1]))
    {
      trading[i]<-"Hol"
      settlement[i]<-"Hol"
      reason[i]<-as.character(holiday[j,3])
    }
  }
}
#####
all<-cbind(char,day,trading,settlement,reason)
file <- tempfile()
x <- matrix(all, nrow =length(char),ncol=5, dimnames = list(c(), c("Date", "Day","For Trading", "For Settlement", "Reason")))
