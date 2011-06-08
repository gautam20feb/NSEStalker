library(tseries)
library(googleVis)
library(fTrading)
library(rjson)

macdATL <-function(numdays,instr)
{
# numdays: no of days for which the data is to be plotted
# instr: the instrument symbol eg. "ONGC.NC"
# to change graph properties see options list of gvisAnnotatedTime at:http://code.google.com/apis/visualization/documentation/gallery/annotatedtimeline.html#Configuration_Options

  nDays <- numdays
  instrument <- instr
  
  start <- strftime(as.POSIXlt(Sys.time() - nDays*24*3600), format="%Y-%m-%d") 
  end <- strftime(as.POSIXlt(Sys.time()), format = "%Y-%m-%d")  
  x <- get.hist.quote(instrument = instrument, start = start, end = end) 
  x<- ts(x)
  temp <- data.frame(x)
  temp1 <- data.frame("MCAD" = macdTA(ts(temp[4])))
  temp2 <- data.frame("CDS" = cdsTA(ts(temp[4])))
  frame1 <- data.frame(attr(x,"index"),temp1,title = "MACD")
  frame2 <- data.frame(attr(x,"index"),temp[4],title = "Close") 
  frame3 <- data.frame(attr(x,"index"),temp[2],title = "CDS")
  names(frame1)[1]<-"Date"
  names(frame1)[2]<-"Value"
  names(frame2)[1]<-"Date"
  names(frame2)[2]<-"Value"
  names(frame3)[1]<-"Date"
  names(frame3)[2]<-"Value"
  frame <- rbind(frame1,frame2,frame3)
  
  # The parameters passed to the list options() are first converted to JSON objects, So we can either pass an array verbatim or a JSON object itself. As illustrated below:
  AnnoTimeLine  <- gvisAnnotatedTimeLine(frame, datevar="Date",
                           numvar="Value", idvar = "title",
                           options=list(displayAnnotations=F,
                             legendPosition="newRow",
                             width=800, 
                             height=600, 
                             scaleType = 'allmaximized',
                             displayExactValues= T,
                             scaleColumns = "[0,1,2]",
                             colors = toJSON(c("blue", "red", "green")))
                             )
                             
                           
  # Display chart
  plot(AnnoTimeLine) 
  # Create Google Gadget
  cat(createGoogleGadget(AnnoTimeLine), file="annotimeline.htm")
  }