\name{generate.urls}
\alias{generate.urls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generate.urls(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (a, b) 
{
    holiday <- read.csv(paste(data_path, "holiday.csv", sep = ""), 
        header = T)
    tS = timeSequence(from = a, to = b, by = "day")
    char <- as.character(tS)
    day <- dayOfWeek(tS)
    trading <- as.character(seq(length(char)))
    settlement <- as.character(seq(length(char)))
    reason <- as.character(seq(length(char)))
    mont <- as.character(seq(length(char)))
    year <- as.character(seq(length(char)))
    date <- as.character(seq(length(char)))
    equity <- as.character(seq(length(char)))
    derivative <- as.character(seq(length(char)))
    atom <- atoms(tS)
    for (i in 1:length(tS)) {
        if (atom[i, 2] == 1) 
            mont[i] <- "JAN"
        if (atom[i, 2] == 2) 
            mont[i] <- "FEB"
        if (atom[i, 2] == 3) 
            mont[i] <- "MAR"
        if (atom[i, 2] == 4) 
            mont[i] <- "APR"
        if (atom[i, 2] == 5) 
            mont[i] <- "MAY"
        if (atom[i, 2] == 6) 
            mont[i] <- "JUN"
        if (atom[i, 2] == 7) 
            mont[i] <- "JUL"
        if (atom[i, 2] == 8) 
            mont[i] <- "AUG"
        if (atom[i, 2] == 9) 
            mont[i] <- "SEP"
        if (atom[i, 2] == 10) 
            mont[i] <- "OCT"
        if (atom[i, 2] == 11) 
            mont[i] <- "NOV"
        if (atom[i, 2] == 12) 
            mont[i] <- "DEC"
        year[i] <- atom[i, 1]
        date[i] <- atom[i, 3]
    }
    for (i in 1:length(char)) {
        if (day[i] == "Sat" || day[i] == "Sun") {
            trading[i] <- "Holiday"
            settlement[i] <- "Holiday"
            reason[i] <- "Weekend"
        }
        else {
            trading[i] <- "Working Day"
            settlement[i] <- "Working Day"
            reason[i] <- ""
        }
        for (j in 1:nrow(holiday)) {
            if (as.character(char[i]) == as.character(holiday[j, 
                1])) {
                trading[i] <- "Holiday"
                settlement[i] <- "Holiday"
                reason[i] <- as.character(holiday[j, 3])
            }
        }
    }
    for (i in 1:length(tS)) {
        if (trading[i] == "Working Day" || settlement[i] == "Working Day") {
            if (as.integer(date[i]) < 10) {
                equity[i] <- as.character(composeURL("www.nseindia.com/content/historical/EQUITIES/", 
                  year[i], "/", mont[i], "/cm0", date[i], mont[i], 
                  year[i], "bhav.csv.zip"))
                derivative[i] <- as.character(composeURL("www.nseindia.com/content/historical/DERIVATIVES/", 
                  year[i], "/", mont[i], "/fo0", date[i], mont[i], 
                  year[i], "bhav.csv.zip"))
            }
            else {
                equity[i] <- as.character(composeURL("www.nseindia.com/content/historical/EQUITIES/", 
                  year[i], "/", mont[i], "/cm", date[i], mont[i], 
                  year[i], "bhav.csv.zip"))
                derivative[i] <- as.character(composeURL("www.nseindia.com/content/historical/DERIVATIVES/", 
                  year[i], "/", mont[i], "/fo", date[i], mont[i], 
                  year[i], "bhav.csv.zip"))
            }
        }
        else {
            equity[i] <- NA
            derivative[i] <- NA
        }
    }
    all <- cbind(char, day, trading, settlement, reason, date, 
        mont, year, equity, derivative)
    x <- matrix(all, nrow = length(char), ncol = 10, dimnames = list(c(), 
        c("Date", "Day", "For Trading", "For Settlement", "Reason", 
            "Date", "Month", "Year", "Equity URL", "Derivative URL")))
    write.csv(x, file = paste(data_path, "list_of_urls.csv", 
        sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
