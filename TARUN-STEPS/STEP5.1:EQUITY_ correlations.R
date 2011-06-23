library (lattice)
library (fBasics)
library (fImport)
library (RMySQL)
m <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE derivedDB")
CreatesEquityPairCorrelation <- function (start.date, end.date){
  query.occurence <- paste ("SELECT DISTINCT SYMBOL, COUNT(*) as OCCURENCE FROM equity_with_futures WHERE
                             TIMESTAMP BETWEEN '", start.date, "' AND '", end.date ,"' GROUP BY SYMBOL", 
                             sep="") 
  query.data <- paste ("SELECT SYMBOL, CLOSE, PREVCLOSE FROM equity_with_futures WHERE TIMESTAMP BETWEEN '", 
                       start.date, "' AND '", end, "'", sep="")
  stock.occurence <- dbGetQuery (con, query.occurence)
  ret.cls.data <- dbGetQuery (con, query.data)
  stock <- as.data.frame (stock.occurence[, 1])
  colnames (stock) <- c ("stock")
  faulty <- which (stock.occurence[, 2] != max (stock.occurence[, 2]))
  faulty.stocks <- stock.occurence[faulty, 1] 
  sapply (faulty.stocks, function (x) ret.cls.data <<- ret.cls.data [(ret.cls.data$SYMBOL) != x, ])
  sapply (faulty.stocks, function (x) stock <<- as.data.frame (stock[(stock$stock) != x, ] ))
  stocks.name <- as.character (stock[, 1])
  assign ("stocks.name", stocks.name, envir=globalenv())
  close.value <- ret.cls.data[2]  
  log.returns <- log (ret.cls.data[2] / ret.cls.data[3])
  log.returns <- as.matrix (round (log.returns, 3))
  dim (log.returns) <- c(max (stock.occurence[, 2]), (length (stock.occurence[, 1]) - length (faulty.stocks)))
  close.value <- as.matrix (round (close.value, 3))
  dim (close.value) <- c(max (stock.occurence[, 2]), (length (stock.occurence[, 1]) - length (faulty.stocks)))
  assign ("log.returns", log.returns, envir = globalenv())
  assign ("close.value", close.value, envir = globalenv())
  len.relevant.stks<-seq(length(stock.occurence[,1])-length(faulty.stocks))
  pairs <- apply (expand.grid (len.relevant.stks, len.relevant.stks), MARGIN = 1, function(x) 
                if (x[1] > x[2]) 
                  c(x[1], x[2]) 
                else 
                  c(NA, NA)
                )
  relevant.pairs <- t (na.omit (t (pairs)))
  relevant.pairs <- as.data.frame (relevant.pairs)
  krownm <- 1
  assign ("krownm", krownm, envir=globalenv())
  sapply (relevant.pairs, CalculatesCorrelationOfPair)  
}

CalculatesCorrelationOfPair <- function (a){
  a <- as.matrix (a)
  # calculates kendall coeffecients
  kendall.results.logreturn  <<- cor.test (log.returns[, a[1]], log.returns[, a[2]], method = "kendall" )
  pearson.results.logreturn  <<- cor.test (log.returns[, a[1]], log.returns[, a[2]])
  spearman.results.logreturn <<- cor.test (log.returns[, a[1]], log.returns[, a[2]], method = "spearman")
  kendall.results.closeval   <<- cor.test (close.value[, a[1]], close.value[, a[2]], method = "kendall" )
  pearson.results.closeval   <<- cor.test (close.value[, a[1]], close.value[, a[2]]) 
  spearman.results.closeval  <<- cor.test (close.value[, a[1]], close.value[, a[2]], method = "spearman")
  print (c(a[1], a[2]))
  print (c(stocks.name[a[1]], stocks.name[a[2]]))
  tobe <<- c(stocks.name[a[1]], stocks.name[a[2]], round (kendall.results.logreturn[[4]], 3), 
             round (pearson.results.logreturn[[4]], 3), round (spearman.results.logreturn[[4]], 3), 
             round (kendall.results.closeval[[4]], 3), round (pearson.results.closeval[[4]], 3),
             round (spearman.results.closeval[[4]], 3))    
  d <- data.frame()
  pair.correlation.row <- rbind(d,tobe)
  names (pair.correlation.row) <- c("stock1","stock2","kend_ret","pears_ret","spear_ret","kend_price","pears_price","spear_price")
  row.names (pair.correlation.row) <- krownm
  krownm <<- krownm+1
  dbWriteTable(con, name = "equity_correlation", value = pair.correlation.row, append = T)
}