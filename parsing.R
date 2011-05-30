library(XML)
doc = xmlRoot(xmlTreeParse("machines"))
tmp = xmlSApply(doc , function(x) xmlSApply(x, xmlValue))
tmp = t(tmp)
machines = as.data.frame(matrix((tmp), 6))
names(machines) = names(doc[[1]])

