library(fImport)
library (bitops)
library (methods)
library(RMySQL)
library(XML)
library(timeDate)

source("../lib/read_machines.R")
source("../lib/create_connection.R")
connection.log <- file(paste("../log/" ,as.character(timestamp()),"log.csv",sep="") , "w")  ##<< For logging the files written to database
config_path <- "../config/"


working_day <- read.csv("./Data/nearest_expiry_date.csv", sep=",", header=T)

machines <- GenerateMachimesDataframe(config_path)
  conn <- CreateConnection("intern@Ophelia" , machines , "TWS_PERMIN_futures")
comp <- c("HDFC","HDFCBANK","BHEL","HINDUNILVR" , "INFOSYSTCH","MANDM","NTPC")
sapply(comp , function(x) calc(x, "10:00:00"))

calc <- function(table , time = "10:00:00")
{
end.ind <- which(working_day$Day == "2011-06-13")

main <- data.frame()
for(i in 1:250)
{
start.ind <- end.ind - 1

end <- paste(as.character(working_day$Day[end.ind]), "  " ,time ,sep="")
start <- paste(as.character(working_day$Day[start.ind]), "  " ,time ,sep="")
try(if(1==1){
q <- paste("SELECT * from ", table , " Where TIMESTAMP BETWEEN '", start ,"' AND '", end ,"' order by TIMESTAMP", sep="")
data <- dbGetQuery(conn, q)
high <- max(data$HIGH)
low <- min(data$LOW)
close <- tail(data$CLOSE, n= 1L)
timestamp <- as.character(working_day$Day[end.ind])
d <- data.frame(table , timestamp , high , low , close)
main <- rbind(main , d)
} , silent = TRUE)
end.ind <- end.ind -1
}
names(main) <- c("CompanyName", "TIMESTAMP" , "HIGH", "LOW" , "CLOSE")
write.table(main ,file=paste(table,".csv",sep=""),sep = ",", row.names= FALSE)
}