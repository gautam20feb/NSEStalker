test.examples <- function(){
  for(i in 1:length(lwday))
  {
    checkEquals(trading[z[i]], "Working Day")
  }
}