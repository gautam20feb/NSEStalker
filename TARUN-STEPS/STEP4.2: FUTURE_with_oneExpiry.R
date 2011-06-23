library (lattice)
library (fBasics)
library (fImport)
library (RMySQL)
m <- dbDriver ("MySQL", max.con = 100)
con.derived <- dbConnect (m, user="root", password = "intern123", host = "localhost", dbname="NSE derivedDB")
con.rawdatabase <- dbConnect (m, user="root", password = "intern123", host = "localhost", dbname="NSE rawdatabase")
CreatesFuturesWithNearestExpiry <- function (start.date, end.date){
  query <- paste ("SELECT INSTRUMENT, SYMBOL, min(EXPIRY_DT), OPEN, HIGH, LOW, CLOSE, SETTLE_PR, CONTRACTS, 
                  VAL_INLAKH, OPEN_INT, CHG_IN_OI, TIMESTAMP FROM future WHERE TIMESTAMP BETWEEN '",
                  start.date, "' AND '", end.date, "' GROUP BY TIMESTAMP, SYMBOL ORDER BY SYMBOL, TIMESTAMP",
                  sep="")
  min.expiry.table <- dbGetQuery (con.rawdatabase, query)
  dbWriteTable (con.derived, name = "future_earliest_expiry_dt", value = min.expiry.table, append=T)
}