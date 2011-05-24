fldrtomysql <- function(connection,folderpath,tablename)
# folderpath: path of the folder including folder name
{
  temp <- getwd()
  setwd(folderpath)
  names <-dir()
  setwd(temp)
  for (name in names)
  {
    string =substr(name,nchar(name)-3,nchar(name))
    if (string == ".csv")  
    rdtomysql(connection,paste(folderpath,"/",name,sep=""),tablename)
  }
}