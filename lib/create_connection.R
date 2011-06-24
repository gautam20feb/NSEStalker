library(RMySQL)

# TO create the MYSQL connections
CreateConnection <- function
### Creates the MySQL connections 
(user.name="intern@Ophelia",
### the machine to which the connection is to be made
machines=machines,
### dataframe with available machines' information i.e. machine name, user name, database, password, host name
database
### List of the databases you want to connect 
){
  
  driver<- dbDriver("MySQL" , max.con = 1000) ##<< Defining the type of connection
  pos = which(machines[,1]==user.name)  ##<< To find which user it is
  usr = unlist(strsplit(user.name,"@"))[1]
  hst = as.character(machines[pos,2])
  pswd = as.character(machines[pos,3])
  connection<- dbConnect(driver , user=usr , password = pswd , host = hst , dbname= database) ##<< sets the connection with the required database
  connection
  ### the created MySQL connection is returned
}