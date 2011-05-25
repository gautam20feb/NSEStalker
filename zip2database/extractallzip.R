extractall <- function(connection,infldrpath,targetfldrpath)
# infldrpath: path of the folder including folder name

{
  temp <- getwd()
  setwd(infldrpath)
  names <-dir()
  setwd(temp)
  for (name in names)
  {
    string =substr(name,nchar(name)-7,nchar(name))
    if (string == ".csv.zip")  
    extract(paste(infldrpath,"/",name,sep = ""),targetfldrpath)
  }
}