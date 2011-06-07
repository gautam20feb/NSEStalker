m <- dbDriver("MySQL", max.con = 25)
conn <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname= "Tws_daily_data") ## sets the connection with the required database
ss="Stocks"
stk<-read.table("STK.with.Sym.csv", sep=",", header=T)

 names(stk)<-c("ID" ,"NSE_NAME","TWS_SYMBOL","NAME_USED")
   ifelse(dbExistsTable(conn, ss),dbWriteTable(conn, name =ss, value=stk, append = T),dbWriteTable(conn, name = ss, value=stk))