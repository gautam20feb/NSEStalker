plotbarchart <- function(numdays, instr)
{
  nDays <- numdays
  instrument <- instr
  
  start <- strftime(as.POSIXlt(Sys.time() - nDays*24*3600), format="%Y-%m-%d") 
  end <- strftime(as.POSIXlt(Sys.time()), format = "%Y-%m-%d")  
  x <- get.hist.quote(instrument = instrument, start = start, end = end) 
  x<- ts(x)
  plotmyOHLC(x, ylab = "price", main = instrument)
}