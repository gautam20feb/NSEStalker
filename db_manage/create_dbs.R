library(RMySQL)
db<-read.csv("./data/Databases.csv", sep=",", header=F) ### Read the Stock Nam
m<- dbDriver("MySQL", max.con = 1000)
doc = xmlRoot(xmlTreeParse("./config/machines.xml")) ##<< parses all of the config file
tmp = xmlSApply(doc , function(x) xmlSApply(x, xmlValue)) ##<< creates a matrix of machines information
tmp = t(tmp) ##<< takes transpose 
n<-nrow(tmp) ##<< get the number of users
machines = as.data.frame(matrix((tmp), n)) ##<< produces a dataframe for the matrix
names(machines) = names(doc[[1]]) ##<< names the corresponding columns
### Function to create the Databases as mentioned in Databases.csv
create.db<-function(
machine.name
### In the format Usernmae@machine
)
{
  usr<-unlist(strsplit(machine.name,"@"))[1]
  pos=which(machines[,1]==machine.name)
  hst=as.character(machines[pos,2])
  pswd=as.character(machines[pos,3])
  con <- dbConnect(m, user=usr, password = pswd, host = hst)
  for(i in 1 : nrow(db))
    {
      try(dbGetQuery(con,paste("create database ",db[i,2],sep="")),silent=TRUE) ## implements the query
    }  
}