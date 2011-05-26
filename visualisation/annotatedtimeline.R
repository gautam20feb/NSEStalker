library(tseries)
annotatedtimeline <-function(numdays,instr)
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
  frame <- data.frame(attr(x,"index"),x)
  names(frame)[1]<-"Date"
  names(frame)[2:5]<-"value"
  open<- data.frame(frame[1],frame[2], title = "Open")
  high<- data.frame(frame[1],frame[3], title = "High")
  low<- data.frame(frame[1],frame[4], title = "Low")
  close<-data.frame(frame[1],frame[5], title = "Close")
  bind <-rbind(open, high, low,close)
AnnoTimeLine  <- gvisAnnotatedTimeLine(bind, datevar="Date",
                           numvar="value", idvar="title",
                           options=list(displayAnnotations=TRUE,
                             legendPosition='newRow',
                             width=800, height=600, scaleType = 'maximized')
                           )
# Display chart
plot(AnnoTimeLine) 
# Create Google Gadget
cat(createGoogleGadget(AnnoTimeLine), file="annotimeline.xml")
}