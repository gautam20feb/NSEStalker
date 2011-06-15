\name{go.db}
\alias{go.db}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
go.db(user = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{user}{
%%     ~~Describe \code{user} here~~
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
function (user = "") 
{
    if (user == "") {
        cat("machine name as *user name*@*machine name*")
        user <- scan(what = "", nmax = 1)
    }
    cat(" 1. Type 1 for downloading from nse and then storing the data as csv files in the ./data/downloaded", 
        "\n", "2. Type 2 for importing all the data from given directory to the database storing equities' data in table equity and derivatives' data in table fo ", 
        "\n", "3. Type 3 for filtering the equities' table i.e. rejecting data for stocks not having futures", 
        "\n", "4. Type 4 for updating", "\n", "5. Type 5 to export data from database as csv files", 
        "\n", "6. Type 6 to import data form csv to database")
    option <- scan(what = "", nmax = 1)
    if (option == 1) {
        cat("enter starting date")
        a <- scan(what = "", nmax = 1)
        cat("enter ending date")
        b <- scan(what = "", nmax = 1)
        get.bhavcopy(a, b, user)
    }
    else if (option == 2) {
        read.to.database.all(user)
    }
    else if (option == 3) {
        cat("enter refrence future table name")
        reftable <- scan(what = "", nmax = 1)
        cat("enter equity table name to be filtered")
        eqtable <- scan(what = "", nmax = 1)
        filterEQ(user, reftable, eqtable)
    }
    else if (option == 4) {
    }
    else if (option == 5) {
        cat("folder to output csv files to")
        folder <- scan(what = "", nmax = 1)
        dbtocsv(user, folder)
    }
    else if (option == 6) {
        cat("folder to take read csv files from")
        folder <- scan(what = "", nmax = 1)
        csvtodb(folder, user)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
