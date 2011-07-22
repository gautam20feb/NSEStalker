library(gregmisc)

# EDIT THE FINAL_SPLITS_BONUS.csv file before this
final <- read.csv("../data/final_splits_bonus.csv" ,sep= "," , header=TRUE)
splits <- final[grep("Split" , final$X,ignore.case= T ),]
for(i in 1:nrow(splits)){
list <- unlist(strsplit(as.character(splits$X[i]),"TO"))
var1 <- list[1]
var2 <- list[2]
var11 <- trim(unlist(strsplit(trim(var1)," ")))
end <- trim(var11[length(var11)])
var21 <- trim(unlist(strsplit(trim(var2)," ")))
start <- trim(var21[1])
splits[i,11] <- start
splits[i,12] <- end
}

bonus <- final[grep("bonus" , final$X,ignore.case= T ),]
for(i in 1:nrow(bonus))
{

list <- unlist(strsplit(as.character(bonus$X[i]),":"))
var1 <- list[1]
var2 <- list[2]
var11 <- trim(unlist(strsplit(trim(var1)," ")))
end <- trim(var11[length(var11)])
var21 <- trim(unlist(strsplit(trim(var2)," ")))
start <- trim(var21[1])
bonus[i,11] <- start
bonus[i,12] <- end
}
xx <- (as.numeric(bonus$V11)+as.numeric(bonus$V12))

bonus2 <- data.frame(as.character(bonus$Series) ,as.character(bonus$No.Delivery.Start.Date), xx,bonus$V12,bonus$V11)
names(bonus2) <- c("SYMBOL", "EX_DATE", "NEW_POS" ,"EXIST" , "BONUS")

write.table(bonus2, "../data/bonus.csv", sep=",",col.names=NA)



xx <- (as.numeric(splits$V12)/as.numeric(splits$V11))

splits2 <- data.frame(as.character(splits$Series) ,as.character(splits$No.Delivery.Start.Date),xx)
names(splits2) <- c("SYMBOL", "EX_DATE","RATIO")

write.table(splits2, "../data/splits.csv", sep=",",col.names=NA)



 
  


