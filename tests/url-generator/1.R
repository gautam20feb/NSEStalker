test.examples <- function(){
  y<-c(0)
  for(i in 1:length(lwday))
  {
    if(as.character(lwday[i])=="2011-05-26") y<-y+1
  }
  checkEquals(y,1)
}
