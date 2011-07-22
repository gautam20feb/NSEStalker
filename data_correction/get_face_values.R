library(gregmisc)
data <- data.frame(read.csv("../data/equity_info.csv",sep=",", header=T))
data2<- data[grep("EQ", data$SERIES,ignore.case=T), ]
futures_list <- read.csv("../data/futures_list.csv", sep=",")
tables <- trim(as.character(futures_list$NSE_SYMBOL))
need.data <- data.frame()
for(i in 1 : length(tables)){
  data1 <- data2[which(data2$SYMBOL == tables[i]), ]
  need.data <- rbind(need.data, data1)
}


final <- data.frame(need.data$SYMBOL , need.data$FACE.VALUE)
names(final) <- c("SYMBOL" , "FACE_VALUE")
write.table(final , "../data/face_value_equity.csv", row.names= FALSE, sep=",")