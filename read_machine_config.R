library(RMySQL)
library(XML)

#what are a and b?
#I have copied code from parse.R here and deleted parse.R
config_machine<-function(a,b)
{
  m <- dbDriver("MySQL", max.con = 25)
  doc = xmlRoot(xmlTreeParse("./config/machines.xml"))
  tmp = xmlSApply(doc , function(x) xmlSApply(x, xmlValue))
  tmp = t(tmp)
  machines = as.data.frame(matrix((tmp), 6))
  names(machines) = names(doc[[1]])

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
con <- dbConnect(m, user=usr, password = pswd, host = hst, dbname= dbnm)   
  
}


