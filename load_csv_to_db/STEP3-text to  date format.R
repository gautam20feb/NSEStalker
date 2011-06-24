library(lattice)
library(fBasics)
library(fImport)
library(RMySQL)
m <- dbDriver("MySQL", max.con = 100)
tk <-function()
{

con <- dbConnect(m, user="root", password = "intern123", host = "localhost", dbname="NSE_raw_database")


query1<-paste("UPDATE future SET TIMESTAMP=STR_TO_DATE(TIMESTAMP,'%d-%M-%Y')")
dbGetQuery(con,query1)

query2<-paste("UPDATE future SET EXPIRY_DT=STR_TO_DATE(EXPIRY_DT,'%d-%M-%Y')")
dbGetQuery(con,query2)
# 
# query3<-paste("UPDATE options SET TIMESTAMP=STR_TO_DATE(TIMESTAMP,'%d-%M-%Y')")
# dbGetQuery(con,query3)
# 
# query4<-paste("UPDATE options SET EXPIRY_DT=STR_TO_DATE(EXPIRY_DT,'%d-%M-%Y')")
# dbGetQuery(con,query4)

query5<-paste("UPDATE equity SET TIMESTAMP=STR_TO_DATE(TIMESTAMP,'%d-%M-%Y')")
dbGetQuery(con,query5)


}