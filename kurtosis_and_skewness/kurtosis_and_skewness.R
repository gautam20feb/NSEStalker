library(betareg)
library(RMySQL)
library(moments)


source("../lib/read_machines.R")
source("../lib/create_connection.R")

connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database

config_path <- "../config/"

CalculateKurtosisForAll <- function(
### Calculate the kurtosis and skewness for the entered table
user.name= "intern@Ophelia" ,
dbname = "TWS_PERMIN_futures"
)
{


machines=GenerateMachimesDataframe(config_path)
connection <- CreateConnection(user.name , machines ,dbname)
tables <- dbListTables(connection)
tablename <- "KURTOSIS_AND_SKEWNESS"
connection2 <- CreateConnection(user.name , machines ,"test")  ##<< table to write to

# x = tables[2]
sapply(tables , function(x) { CalculateKurtosisForOneStock(connection, connection2 ,tablename , x)})

}

CalculateKurtosisForOneStock <- function(
### Calculate the kurtosis and skewness for the entered table
connection.database.to.read = connection ,
connection.database.to.write = connection2 ,
tablename.to.write = tablename ,
name = x
)
{
print(name)
conn <- connection.database.to.write
query<-paste("SELECT `CLOSE` FROM ", name , sep = "")
spread <-dbGetQuery(connection.database.to.read , query)[,1]
# anscombe.test.for.kurtosis <- anscombe.test(spread, alternative = c("two.sided", "less", "greater"))
kurtosis.value <- kurtosis(spread)
# x.nif <- agostino.test(nif , alternative = c("two.sided", "less", "greater"))
skewness.value <- skewness(spread , na.rm=FALSE)
jarque.test.value <- jarque.test(spread)
data <- data.frame()
data <- data.frame(cbind(name , kurtosis.value , skewness.value ))
names(data) <- c("SYMBOL","KURTOSIS","SKEWNESS")
ifelse(dbExistsTable(conn, tablename.to.write),dbWriteTable(conn, name = tablename.to.write , value=data, append = T),dbWriteTable(conn, name = tablename.to.write, value=data))
}