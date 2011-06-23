library(timeDate)
library(fBasics)
library(fImport)

holiday <-read.csv ("../data/holiday.csv", header = F) # Holiday database
tS = timeSequence (from = "2001-01-01", to = "2011-12-31", by = "day")  # Creating time sequence by day
tSm = timeSequence(from = "2001-01-01", to = "2011-12-31", by = "month")  # Creating timesequence by month

### Initialization
char <- as.character (tS) # Time sequence
day <- dayOfWeek (tS)
trading <- as.character (seq (length (char)))
settlement <- as.character (seq (length (char)))
reason <- as.character (seq (length (char)))
mont <- as.character (seq (length (char)))
year <- as.character (seq (length (char)))
date <- as.character (seq (length (char)))

### Atomizing time-sequence and changing its month to characters
atom <- atoms (tS)
for (i in 1 : length (tS)) {
  if (atom[i,2] == 1) mont[i] <- "JAN"
  if (atom[i,2] == 2) mont[i] <- "FEB"
  if (atom[i,2] == 3) mont[i] <- "MAR"
  if (atom[i,2] == 4) mont[i] <- "APR"
  if (atom[i,2] == 5) mont[i] <- "MAY"
  if (atom[i,2] == 6) mont[i] <- "JUN"
  if (atom[i,2] == 7) mont[i] <- "JUL"
  if (atom[i,2] == 8) mont[i] <- "AUG"
  if (atom[i,2] == 9) mont[i] <- "SEP"
  if (atom[i,2] == 10) mont[i] <- "OCT"
  if (atom[i,2] == 11) mont[i] <- "NOV"
  if (atom[i,2] == 12) mont[i] <- "DEC"
  year[i] <- atom[i,1]
  date[i] <- atom[i,3]  # 'atom' gives the year, date, month dataframe
}

### Creating list of holidays adn working days
for (i in 1 : length (char)) {
  if (day[i] == "Sat" || day[i] == "Sun"){
    trading[i] <- "Holiday"
    settlement[i] <- "Holiday"
    reason[i] <- "Weekend"
  }
  else{
    trading[i] <- "Working Day"
    settlement[i] <- "Working Day"
    reason[i] <- "-"
  }
  for (j in 1 : nrow (holiday)){
    if (as.character (char[i]) == as.character (holiday[j,1])) {
      trading[i] <- "Holiday"
      settlement[i] <- "Holiday"
      reason[i] <- as.character (holiday[j,3])
    }
  }
}

all <- cbind (char, day, trading, settlement, reason, date, mont, year)
x <- matrix (all, nrow = length (char), ncol = 10, dimnames = list (c(), c("Date", "Day","For Trading", "For Settlement", "Reason", "Date", "Month","Year","Equity URL","Derivative URL")))

############################### Last Working Day ###########################################
lwday <- timeLastNdayInMonth (tSm, nday = 4)
z <- seq (length (lwday))
for (j in 1 : length (lwday)) z[j]<-which(x==as.character(lwday[j]))
for(i in 1:length(lwday)) {
  while(trading[z[i]]=="Holiday") {
    lwday[i] <- as.Date (lwday[i]-1)
    z[i] <- (z[i]-1)
  }
}
write.csv(lwday,file="./data/lwday.csv")


