library(RMySQL)

config_machine<-function(a,b)
{
  m <- dbDriver("MySQL", max.con = 25)
  source("parsing.R")
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


