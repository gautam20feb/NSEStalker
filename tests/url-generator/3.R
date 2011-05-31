test.examples <- function(){
  y<-c(0)
  for(i in 1:length(lwday))
  {
    if(day[z[i]+1] =="Fri"||trading[z[i]+1]=="Holiday") 
     {
       y<-y+1
     }
  }
  checkEquals(y,length(lwday))
}