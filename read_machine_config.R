library(RMySQL)
library(XML)

#a:
#I have copied code from parse.R here and deleted parse.R
config_machine<-function(
a,
### a takes type of parameter as input. Should be among:"user","host","dbname","password". 
b
### b take the value of parameter,like config_machine("user","Ophelia")
)
{
  m <- dbDriver("MySQL", max.con = 25)
  doc = xmlRoot(xmlTreeParse("./config/machines.xml")) ## parses all of the config file
  tmp = xmlSApply(doc , function(x) xmlSApply(x, xmlValue)) ## creates a matrix of machines information
  tmp = t(tmp) ## takes transpose
  machines = as.data.frame(matrix((tmp), 6))## produces a dataframe for the matrix
  names(machines) = names(doc[[1]]) ## names the corresponding columns
### searches for b in column a and sets the value of all other machine information correspondingly
  if(a=="user")
  {
    pos=which(machines[,1]==b)
    usr=as.character(machines[pos,1])
    hst=as.character(machines[pos,2])
    dbnm=as.character(machines[pos,3])
    pswd=as.character(machines[pos,4])
  }
  else if(a=="host")
  {
    pos=which(machines[,2]==b)
    usr=as.character(machines[pos,1])
    hst=as.character(machines[pos,2])
    dbnm=as.character(machines[pos,3])
    pswd=as.character(machines[pos,4])
  }
  else if(a=="dbname")
  {
    pos=which(machines[,3]==b)
    usr=as.character(machines[pos,1])
    hst=as.character(machines[pos,2])
    dbnm=as.character(machines[pos,3])
    pswd=as.character(machines[pos,4])
  }
  else if(a=="password")
  {
    pos=which(machines[,4]==b)
    usr=as.character(machines[pos,1])
    hst=as.character(machines[pos,2])
    dbnm=as.character(machines[pos,3])
    pswd=as.character(machines[pos,4])
  }
con <- dbConnect(m, user=usr, password = pswd, host = hst, dbname= dbnm) ## sets the connection with
# the required database
  
}


