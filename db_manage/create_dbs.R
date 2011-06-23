library(RMySQL)

source("../lib/create_connection.R")
source("../lib/read_machines.R")

config_path <- "../config/"

# Creates all the required databases
CreateAllDatabases <-function(
user.name="intern@Ophelia"
### In the format Usernmae@machine
)
{ 
  db<-read.csv("../data/Databases.csv", sep=",", header=F) ##<< Read the Stock Nam
  m<- dbDriver("MySQL", max.con = 1000)

  machines = GenerateMachimesDataframe(config_path)
  usr <- unlist(strsplit(user.name,"@"))[1]
  pos <- which(machines[,1] == user.name)
  hst <- as.character(machines[pos,2])
  pswd <- as.character(machines[pos,3])
  con <- dbConnect(m, user=usr, password = pswd, host = hst)
  for(i in 1 : nrow(db))
    {
      try(dbGetQuery(con,paste("create database ",db[i,2],sep="")),silent=TRUE) ##<< implements the query
    }  
}