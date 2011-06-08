library(tseries)
library(googleVis)
emaATL <-function(numdays,instr,lambda = 0.5,startup = 1)
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
  print(temp)
  temp <- data.frame("Open" = emaTA(ts(temp[1]),lambda= lambda, startup = startup),
                     "High" = emaTA(ts(temp[2]),lambda= lambda, startup = startup),
                     "Low" = emaTA(ts(temp[3]),lambda= lambda, startup = startup),
                     "Close" = emaTA(ts(temp[4]),lambda= lambda, startup = startup))
  print(temp)
  frame <- data.frame(attr(x,"index"),temp)
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