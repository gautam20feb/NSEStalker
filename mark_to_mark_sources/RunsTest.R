RunsTest <- function(series){
  curr <- series[1]
  pos <- ifelse(curr > 0 , 1 , 0)
  neg <- ifelse(curr < 0 , -1 , 0)
  if(curr == 0)
    { 
    pos <- 1 
    curr <- 1 
    }
 
  run <- c()
  for(i in 2 : length(series))
  { 
    new.curr <- series[i]
    if(new.curr == 0)
      {
      if( pos >= 1)
        new.curr <- -1
      else if( neg <= -1)
        new.curr <- 1
    }
        
    if(new.curr < 0 && curr < 0 )
    {
      neg <- neg - 1
      curr <- new.curr     
    }
    else if(new.curr >  0 && curr < 0 )
    {
      run <- c(run,neg)
      neg <- 0
      pos <- 1
      curr <- new.curr
    }
     else if(new.curr > 0 && curr > 0 )
    {
      pos <- pos + 1
      neg <- 0
      curr <- new.curr
    }
     else if(new.curr < 0 && curr > 0 )
    {
      run  <- c(run,pos)
      pos <- 0
      neg <- -1
      curr <- new.curr
    }
  }
  if(pos >= 1)
    return(c(run,pos))
  
  else if(neg <= -1)
    return (c(run, neg))
}
