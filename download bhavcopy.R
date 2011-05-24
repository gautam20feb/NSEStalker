library("RCurl")
library (bitops)
library (methods)
bhavcopy <- function()
{
sURL <-"http://www.nseindia.com/content/historical/EQUITIES/2011/MAY/cm23MAY2011bhav.csv.zip" 

options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6;en-US; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12")
tContent <- getURLContent(sURL, verbose = TRUE, useragent = getOption("HTTPUserAgent"))
attributes(tContent) = NULL 

writeBin(tContent, "test.zip")
}