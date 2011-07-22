library(gregmisc)
data <- data.frame(read.csv("../data/corporate_actions.csv",sep=",", header=T))


data2<- data[grep("EQ", data$Series,ignore.case=T), ]


futures_list <- read.csv("../data/futures_list.csv", sep=",")
tables <- trim(as.character(futures_list$NSE_SYMBOL))
need.data <- data.frame()
for(i in 1 : length(tables))
  {
  data1 <- data2[which(data2$Symbol== tables[i]), ]
  need.data <- rbind(need.data, data1)
}
final1 <- need.data[grep("split",need.data$Purpose,ignore.case = T ),]
final2 <- need.data[grep("bonus",need.data$Purpose,ignore.case = T ),]
final <- rbind(final1,final2)
write.table(final , "../data/final_splits_bonus.csv", sep = ",", row.names= FALSE)
  