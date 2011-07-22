# div <- split(dividend , dividend$Symbol)
# Copy face Values to futures_list
dividends <- read.csv("1ll_dividends.csv", sep = ",")
fut_list <- trim(read.csv("../data/futures_list.csv", sep=","))
face.value <- data.frame(fut_list$NSE_SYMBOL , fut_list$FACE_VALUE)


percen.ind <- grep( "%" , dividends$PURPOSE , ignore.case = T )
for(i in percen.ind)
{

  comp <- as.character(dividends$SYMBOL[i])
  fv <- face.value$fut_list.FACE_VALUE[which(face.value$fut_list.SYMBOL == comp)] 
  p <- as.numeric(unlist(strsplit(as.character(dividends$VALUE[i]), "%"))[1])
  div <- p/100 * fv
  dividends[i,5] <- div
}

for( j in 1: nrow(dividends))
{
  if(is.na(dividends$V5[j]))
  {
    dividends$V5[j] <- as.character(dividends$VALUE[j])
  }
}
  dividends$PURPOSE <- NULL
  dividends$VALUE <- NULL
  names(dividends) <- c("SYMBOL" , "EX_DATE", "DIVIDEND")
write.table(dividends , "all_dividends.csv",sep=",",row.names= FALSE)