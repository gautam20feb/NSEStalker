\name{download.to.database}
\alias{download.to.database}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
download.to.database(a, b, user.name = "root@localhost")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{user.name}{
%%     ~~Describe \code{user.name} here~~
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
function (a, b, user.name = "root@localhost") 
{
    conn <- create_connection(user.name)
    generate.urls(a, b)
    mylog <- file(paste(log_path, "downloading.log.csv", sep = ""), 
        "w")
    mylog2 <- file(paste(log_path, "database.log.csv", sep = ""), 
        "w")
    mylog3 <- file(paste(log_path, "error.log.csv", sep = ""), 
        "w")
    sapply(equity, download.STK)
    sapply(derivative, download.FUT.OPT)
    dir <- getwd()
    extract.all.files(dir, dir)
    close(mylog)
    read.to.database.all(conn, w)
    close(mylog2)
    close(mylog3)
    dbDisconnect(conn)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
