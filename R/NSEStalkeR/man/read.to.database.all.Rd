\name{read.to.database.all}
\alias{read.to.database.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.to.database.all(user, folderpath = paste(data_path, "downloaded", sep = ""))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{user}{
%%     ~~Describe \code{user} here~~
}
  \item{folderpath}{
%%     ~~Describe \code{folderpath} here~~
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
function (user, folderpath = paste(data_path, "downloaded", sep = "")) 
{
    machines <- gen.machines.dataframe()
    conn <- create.connection(user, machines)
    temp <- getwd()
    setwd(folderpath)
    names <- dir()
    setwd(temp)
    for (name in names) {
        type <- substr(name, 1, 2)
        string = substr(name, nchar(name) - 3, nchar(name))
        if (string == ".csv") {
            if (type == "cm") {
                read.to.database.one.file(conn, paste(folderpath, 
                  "/", name, sep = ""), "equity")
            }
            if (type == "fo") {
                read.to.database.one.file(conn, paste(folderpath, 
                  "/", name, sep = ""), "fo")
            }
        }
    }
    dbDisconnect(conn)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
