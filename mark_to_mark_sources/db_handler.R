# This function handles the remote database in phpmyadmin and returns mean, variance, spread, beta and runs in given period
DatabaseHandler <- function (company.1,   # name of any one company in a pair
                             company.2,   # name of the second company 
                             date,        # current date when likeliness of trading is examined
                             window)      # number of days before the current day, whose data are considered for analyzing current status  
{
  m <- dbDriver ("MySQL")    # driver for MySQL server
  conn <- dbConnect(m, dbname = "CORRECTED_OHLC_futures", user = "intern", password = "intern123", host = "192.168.1.106")
   # connection with MySQL server
  dbListTables(conn)
  
  end.date <- as.character(as.Date(date))
  start.date <- as.character(as.Date(date) - window -1 )
  
   query <- paste ("SELECT `TIMESTAMP`, `CLOSE` FROM ",company.1," WHERE `TIMESTAMP` BETWEEN '",start.date,"' AND '",end.date,"' GROUP BY TIMESTAMP", sep = "")
  data1 <- dbGetQuery (conn, query)
  
   query <- paste ("SELECT `TIMESTAMP`, `CLOSE` FROM ",company.2 ," WHERE `TIMESTAMP` BETWEEN '",start.date,"' AND '",end.date,"' GROUP BY TIMESTAMP", sep = "")
  data2 <- dbGetQuery(conn , query)
  
  p1 <- data1$CLOSE[nrow(data1)]
  p2 <- data2$CLOSE[nrow(data2)]
  
  data1 <- data1[ 1 : (nrow(data1)-1),]
  data2 <- data2[ 1 : (nrow(data2)-1),]
  
  sapply(setdiff(data1[,1],data2[,1]),function(x) data1<<-data1[(data1$TIMESTAMP)!= x,])
  sapply(setdiff(data2[,1],data1[,1]),function(x) data2<<-data2[(data2$TIMESTAMP)!= x,])
      
  data1<-data1[order(data1$TIMESTAMP),]
  data2<-data2[order(data2$TIMESTAMP),]
  
  pair <- data.frame (data1$TIMESTAMP,data1$CLOSE, data2$CLOSE)
  names(pair) <- c("TIMESTAMP","company1.CLOSE","company2.CLOSE")
  
  beta.lm <- lm (company1.CLOSE ~ company2.CLOSE, data = pair)
  
  beta <- as.numeric(coefficients(beta.lm)[2])
  intercept <- as.numeric(coefficients(beta.lm)[1])
  spread <- residuals(beta.lm)
  
  curr.spread <- as.double(spread[length(spread)])
  
  spread.mean <- mean(spread)
  sd.spread <- sd(spread) 
  
  runs <- abs(RunsTest(spread))
  mean.runs <- summary(runs)[[4]]
  sd.run <-  sd(runs)
  curr.runs <- as.numeric(tail(runs , n=1L))
  ## disconnecting all existing connections including those on multicore
  dbDisconnect(conn)
  for(co in dbListConnections(m)) dbDisconnect(co)
  dbUnloadDriver(m)
  return(list(spread.mean, sd.spread, mean.runs, sd.run, curr.spread, curr.runs,spread,p1,p2,beta))
}
