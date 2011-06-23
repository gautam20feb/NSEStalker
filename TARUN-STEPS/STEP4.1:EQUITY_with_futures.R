library (lattice)
library (fBasics)
library (fImport)
library (RMySQL)
m <- dbDriver("MySQL", max.con = 100)
con.derived.db <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE derivedDB")
con.rawdatabase <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE rawdatabase")
CreatesEquityWithFutures <- function (start.date, end.date){
  list.of.stocks <- read.csv ("STK.csv")
  string.stocks = ""
  sapply (list.of.stocks[,2], function (x) string.stocks <<- paste (string.stocks, ",'", x, "'", sep=""))
  string.stocks <- substr (string.stocks, 2, nchar (string.stocks))
  query <- paste("SELECT SYMBOL, CLOSE, PREVCLOSE, TIMESTAMP FROM equity WHERE TIMESTAMP BETWEEN '", start.date, "' AND '", end.date, "' AND SYMBOL IN", "(", string.stocks, ")", " AND SERIES ='EQ' ORDER BY SYMBOL, TIMESTAMP", sep="")
  tab <- dbGetQuery (con.rawdatabase, query)
  dbWriteTable (con.derived.db, name = "equity_with_futures", value = tab, append = T)
}