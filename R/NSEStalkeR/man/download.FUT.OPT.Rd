\name{download.FUT.OPT}
\alias{download.FUT.OPT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
download.FUT.OPT(sURL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sURL}{
%%     ~~Describe \code{sURL} here~~
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
function (sURL) 
{
    mylog <- file(paste(log_path, "downloading.log.csv", sep = ""), 
        "a")
    mylog3 <- file(paste(log_path, "error.log.csv", sep = ""), 
        "a")
    tryCatch(if (!is.na(sURL)) {
        cat(as.character(timestamp()), "  downloading ", sURL, 
            "\n", file = mylog, sep = ",")
        options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6;en-US; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12")
        tContent <- getURLContent(sURL, verbose = TRUE, useragent = getOption("HTTPUserAgent"))
        attributes(tContent) = NULL
        t <- nchar(sURL)
        c <- substr(sURL, t - 22, t)
        writeBin(tContent, c)
    }, error = function(e) {
        cat(as.character(timestamp()), "  URL NOT FOUND ", sURL, 
            "\n", file = mylog3, sep = ",")
    })
    close(mylog)
    close(mylog3)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
