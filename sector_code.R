library(gregmisc)      #to trim the end spaces in NSE data names
# library(Deducer)      to sort the column
setwd("~/projects/NSEStalker/data")
futures_list <- read.csv("~/projects/NSEStalker/data/futures_list.csv", header=F  # Importing the 2 files
sector <- read.csv("~/projects/NSEStalker/data/sector.csv", header=F)             #   -do-
x<-trim(as.character(futures_list[,2]))
y<-trim(as.character(sector[,3]))

z<-intersect(x,y)                # common names in x and y
for(i in 1:length(z))
{
  a<-which(x==z[i])
  b<-which(y==z[i])
  Futures_list[a,3]<-sector[b,1]
 }
 
newdata <- sectored_futures[order(sectored_futures$"SECTOR"),]    # sort by sector
#  all<-sort(Futures_list, by ="V3")
write.table(newdata, file="sectored_futures.csv",sep=",")