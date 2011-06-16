library(RMySQL)
db<-read.csv("./data/Databases.csv", sep=",", header=F) ### Read the Stock Nam
n <- dbDriver("MySQL", max.con = 100)
con <- dbConnect(n, user="gautam", password = "intern123", host = "localhost")
sapply(db[,2],create.db)
    
create.db<-function(x)
{
  dbGetQuery(con,paste("create database ",x,sep="")) ## implements the query
}