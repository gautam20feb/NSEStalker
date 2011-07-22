splits <- read.csv("../data/splits.csv", row.names= NULL)
bonus <- read.csv("../data/bonus.csv" , , row.names= NULL)
library(gregmisc)
t<- data.frame()
for(i in 1 : nrow(splits))
{ 
  date <- trim(as.character(splits$EX_DATE[i]))
  temp <- unlist(strsplit(date,"/"))
  new.date <- paste(temp[3],temp[2],temp[1], sep="-")
  splits[i,5] <- new.date
  }
  splits$EX_DATE <- NULL
  names(splits) <- c("X","SYMBOL", "RATIO", "EX_DATE")

for(i in 1 : nrow(bonus))
{
  date <- trim(as.character(bonus$EX_DATE[i]))
  temp <- unlist(strsplit(date,"/"))
  new.date <- paste(temp[3],temp[2],temp[1], sep="-")
  bonus[i,7] <- new.date
  }
  bonus$EX_DATE <- NULL
  names(bonus) <- c("X","SYMBOL","NEW_POS" ,"EXIST","BONUS" ,"EX_DATE")
write.table(bonus, "bonus2.csv", sep=",",col.names=NA)
write.table(splits, "splits2.csv", sep=",",col.names=NA)

