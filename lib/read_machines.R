library(XML)

GenerateMachimesDataframe <- function(
### Reads the machines.xml file and returns a dataframe with available machines' information user name,password, host name 
config_path = "../config/"
### The path of machines.xml file. The default is "../config/"
){
  doc.xml = xmlRoot(xmlTreeParse(paste(config_path , "machines.xml", sep = ""))) ##<< parses all of the config file
  xml.matrix = xmlSApply(doc.xml , function(x) xmlSApply(x , xmlValue))  ##<< creates a matrix of machines information
  xml.matrix = t(xml.matrix) ##<< takes transpose of matrix to make it standard
  number.of.users <- nrow(xml.matrix) ##<< get the number of users
  machines = as.data.frame(matrix((xml.matrix), number.of.users)) ##<< produces a dataframe for the matrix
  names(machines) = names(doc.xml[[1]]) ##<< names the corresponding columns
  machines
  ### machines is the data frame
}

