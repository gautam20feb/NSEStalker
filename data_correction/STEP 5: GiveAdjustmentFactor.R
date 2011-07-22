# setwd("~/Dropbox/Intern/correction")
library(timeSeries)
library(xts)
library(RMySQL)
library(gregmisc)
bonus <- read.csv("bonus2.csv")
splits <- read.csv("splits2.csv")
TIMELINE_futures <- trim(read.csv("TIMELINE_futures.csv"))
nearest_expiry_date <- trim(read.csv("nearest_expiry_date.csv"))
futures_list <- trim(read.csv("futures_list.csv", sep=","))
dividends <- trim(read.csv("all_dividends.csv"))

tables <- trim(as.character(futures_list$TABLE_NAME))
m <- dbDriver("MySQL")
conn <- dbConnect(m, dbname = "ADJUSTMENT_FACTORS", user = "intern", password = "intern123", host = "192.168.1.106")

CalculateAllAdjFactors <- function()
{
  
  for(it in 1: length(tables))
  {
  data <- GiveAdjustmentFactorTable(tables[it])
  dbWriteTable(conn ,name = tables[it], value = data)
  }
}

GiveAdjustmentFactorTable <- function(table)       # takes input of company name and calculates adjusting vector
{ 
#   table= "3IINFOTECH"
  ind <- which(TIMELINE_futures$SYMBOL == table)
 start.date <- as.character(TIMELINE_futures[ind,2])
 end.date <- as.character(Sys.Date())


 data <- nearest_expiry_date[which(nearest_expiry_date[,2] == start.date):which(nearest_expiry_date[,2] == end.date),2]
 data2 <- as.vector(data)
 n <- length(data2)
 ts <- timeSeries(data2)
 final.data <- data.frame(data2 , 1,0)
 names(final.data) <- c("TIMESTAMP","ADJ_FACTOR_MUL", "ADJ_FACTOR_ADD")

if(!is.na(TIMELINE_futures[ind , 4]))
{
  break.start <- as.character(TIMELINE_futures$BREAK_START[ind])
  break.end <- as.character(TIMELINE_futures$BREAK_END[ind])
  start.ind <- which(final.data$TIMESTAMP == break.start )
  end.ind <- which(final.data$TIMESTAMP == break.end )
  final.data$ADJ_FACTOR_MUL[start.ind : end.ind ] <- 0
  final.data$ADJ_FACTOR_ADD[start.ind : end.ind ] <- 0
 
  
}
  
##################################################################  
dividend <- dividends[which(dividends$SYMBOL == table), ]
div <- dividend[order(dividend$EX_DATE),]
if(nrow(div)>0)
{
  for( j in 1 : nrow(div))
  {
    try( 
      if(1==1)
    {
    index <- which(as.character(final.data$TIMESTAMP) == as.character(div$EX_DATE[j]))
    val <- as.double(as.character(div$DIVIDEND[j]))
    final.data$ADJ_FACTOR_ADD[index : n ] <-  final.data$ADJ_FACTOR_ADD[index : n ] + val
    
  }, silent = TRUE)
} 
}


split <- splits[which(splits$SYMBOL == table), ]

if(nrow(split)>0)
{
  for( j in 1 : nrow(split))
  {
    try( if(1==1)
    {
    index <- which(as.character(final.data$TIMESTAMP) == split$EX_DATE[j])
    x <- split$RATIO[j]
    final.data$ADJ_FACTOR_MUL[index : n ] <- as.double(final.data$ADJ_FACTOR_MUL[index : n ]) * x
    
  }, silent = TRUE)
} 
}
bon <- bonus[which(bonus$SYMBOL == table),]

if(nrow(bon)>0)
{
  for( j in 1 : nrow(bon))
  {
    try(
      if(1==1) 
        {
    index <- which(as.character(final.data$TIMESTAMP) == (bon$EX_DATE[j]))
    x <- bon$EXIST[j]
    y <- bon$NEW_POS[j]
    final.data$ADJ_FACTOR_MUL[index : n ] <- final.data$ADJ_FACTOR_MUL[index : n ] * (y/x)
    
  }, silent= TRUE)
}
}
if(!is.na(TIMELINE_futures[ind , 4]))
{
  break.start <- as.character(TIMELINE_futures$BREAK_START[ind])
  break.end <- as.character(TIMELINE_futures$BREAK_END[ind])
  start.ind <- which(final.data$TIMESTAMP == break.start )
  end.ind <- which(final.data$TIMESTAMP == break.end )
  final.data$ADJ_FACTOR_MUL[start.ind : end.ind ] <- 0
  final.data$ADJ_FACTOR_ADD[start.ind : end.ind ] <- 0
 
  
}
return(final.data)
}