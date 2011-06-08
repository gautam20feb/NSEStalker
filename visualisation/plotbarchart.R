library(tseries)
library(fTrading)
plotbarchart <- function(numdays, instr)
# numdays: no of days for which the data is to plotted
# instr: the instrument symbol eg. "ONGC.NC"
# function plots the open/high/low/close bar chart for the data correspoding to the instrument 
# instr for numdays no of days

{
  nDays <- numdays
  instrument <- instr
  
  start <- strftime(as.POSIXlt(Sys.time() - nDays*24*3600), format="%Y-%m-%d") 
  print(start)
   end <- strftime(as.POSIXlt(Sys.time()), format = "%Y-%m-%d")
     print(end)
  x <- get.hist.quote(instrument = instrument, start = start, end = end) 
 
  x<- ts(x)
  print(x)
 plotmyOHLC(x, ylab = "price", main = instrument)
}