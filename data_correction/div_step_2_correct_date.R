########### Before running this code 
# Rename the NSE Symbols to the standard Table names
# In the Purpose column:
#   1. Replace all "/-" by " "(space)
#   2. Remove "PER SHARE" 
#   3. Remove "PER SH"
#   4. Remove "AGM"
#   5. Remove "ANNUALGENERAL MEETING"
#   6. Remove "INTERIM"
#   7. Remove "FINAL"
#   8. Remove "AND"
#   9. Remove "PURPOSE"
#   10.Remove "REVISED"
#   11.Remove "ELECTION"
#   12.Remove "DIRECTOR"
#   13.Remove "1st", "2nd","3rd" , 
#   14.Replace "-" by " "
#   15. Replace all "Rs " by "Rs."
# 
# 





dividend <- trim(read.csv("../data/dividends.csv", row.names= NULL))






library(gregmisc)
for(i in 1 : nrow(dividend))
{ 
  date <- trim(as.character(dividend$Ex.date[i]))
  temp <- unlist(strsplit(date,"/"))
  new.date <- paste(temp[3],temp[2],temp[1], sep="-")
  dividend[i,6]  <- new.date
  }
  dividend$Ex.date <- NULL
  dividend$Series <- NULL
  names(dividend) <- c("SYMBOL", "PURPOSE","VALUE", "EX_DATE")
  write.table(dividend , "dividends2.csv", row.names= FALSE, sep=",")

