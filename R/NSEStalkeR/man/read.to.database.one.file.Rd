\name{read.to.database.one.file}
\alias{read.to.database.one.file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.to.database.one.file(conn, filename, tablename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{tablename}{
%%     ~~Describe \code{tablename} here~~
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
function (conn, filename, tablename) 
{
    data <- read.table(filename, header = T, sep = ",")
    data$X <- NULL
    mylog2 <- file(paste(log_path, "database.log.csv", sep = ""), 
        "a")
    if (tablename == "equity") {
        ifelse(dbExistsTable(conn, tablename), dbWriteTable(conn, 
            name = tablename, value = data, append = T), dbWriteTable(conn, 
            name = tablename, value = data))
        cat(as.character(timestamp()), "added to equity ", filename, 
            "\n", file = mylog2, sep = ",")
    }
    if (tablename == "fo") {
        datao1 <- data[grep("OPTIDX", data$INSTRUMENT, ignore.case = T), 
            ]
        datao2 <- data[grep("OPTSTK", data$INSTRUMENT, ignore.case = T), 
            ]
        dataf1 <- data[grep("FUTIDX", data$INSTRUMENT, ignore.case = T), 
            ]
        dataf2 <- data[grep("FUTINT", data$INSTRUMENT, ignore.case = T), 
            ]
        dataf3 <- data[grep("FUTSTK", data$INSTRUMENT, ignore.case = T), 
            ]
        if ((nrow(dataf1) + nrow(dataf2) + nrow(dataf3) + nrow(datao1) + 
            nrow(datao2)) != nrow(data)) {
            cat(as.character(timestamp()), "  Error in Data ", 
                filename, "\n", file = mylog3, sep = ",")
        }
        if (nrow(datao1) > 0) {
            ifelse(dbExistsTable(conn, "options"), dbWriteTable(conn, 
                name = "options", value = datao1, append = T), 
                dbWriteTable(conn, name = "options", value = datao1))
            cat(as.character(timestamp()), "added Options Index to options ", 
                filename, "\n", file = mylog2, sep = ",")
        }
        if (nrow(datao2) > 0) {
            ifelse(dbExistsTable(conn, "options"), dbWriteTable(conn, 
                name = "options", value = datao2, append = T), 
                dbWriteTable(conn, name = "options", value = datao2))
            cat(as.character(timestamp()), "added Options Stock to options ", 
                filename, "\n", file = mylog2, sep = ",")
        }
        if (nrow(dataf1) > 0) {
            dataf1$STRIKE_PR <- NULL
            dataf1$OPTION_TYP <- NULL
            ifelse(dbExistsTable(conn, "future"), dbWriteTable(conn, 
                name = "future", value = dataf1, append = T), 
                dbWriteTable(conn, name = "future", value = dataf1))
            cat(as.character(timestamp()), "added Future Index to table Future ", 
                filename, "\n", file = mylog2, sep = ",")
        }
        if (nrow(dataf2) > 0) {
            dataf2$STRIKE_PR <- NULL
            dataf2$OPTION_TYP <- NULL
            ifelse(dbExistsTable(conn, "future"), dbWriteTable(conn, 
                name = "future", value = dataf2, append = T), 
                dbWriteTable(conn, name = "future", value = dataf2))
            cat(as.character(timestamp()), "added Future Int to table Future ", 
                filename, "\n", file = mylog2, sep = ",")
        }
        if (nrow(dataf3) > 0) {
            dataf3$STRIKE_PR <- NULL
            dataf3$OPTION_TYP <- NULL
            ifelse(dbExistsTable(conn, "future"), dbWriteTable(conn, 
                name = "future", value = dataf3, append = T), 
                dbWriteTable(conn, name = "future", value = dataf3))
            cat(as.character(timestamp()), "added Future Stock to table future ", 
                filename, "\n", file = mylog2, sep = ",")
        }
    }
    close(mylog2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
