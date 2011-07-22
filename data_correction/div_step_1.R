library(gregmisc)
data <- data.frame(read.csv("../data/corporate_actions.csv",sep=",", header=T))
data2<- data[grep("EQ", data$Series,ignore.case=T), ]
futures_list <- read.csv("../data/futures_list.csv", sep=",")
tables <- trim(as.character(futures_list$NSE_SYMBOL))
need.data <- data.frame()
for(i in 1 : length(tables)){
  data1 <- data2[which(data2$Symbol == tables[i]), ]
  need.data <- rbind(need.data, data1)
}


eq<- need.data[grep("EQ", need.data$Series,ignore.case=T), ]

div1 <- eq[grep("DIV" ,eq$Purpose,ignore.case=T),]

final <- data.frame(div1$Symbol, div1$Ex.Date , div1$Purpose , div1$Series)
names(final) <- c("Symbol", "Ex.date" , "Purpose", "Series")
swrite.table(final , "dividends.csv", row.names= FALSE, sep=",")