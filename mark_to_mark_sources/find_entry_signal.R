## This function takes arguments for entering into trade and returns pair's residual and spread when trade is opened
FindEntrySignal <- function (company.1,  # first company in pair
                             company.2,  # secondcompany in pair
                             date,       # current date when likeliness of trading is examined
                             window = 365,## number of days before the current day, whose data are considered for analyzing current status
                             tolerance.spread = 2, # coefficient of sigma, standard deviation for spread
                             tolerance.run = 2)  # coefficient of sigma, standard deviation for runs
  { 
  db.handler.return <- DatabaseHandler (company.1 ,company.2 ,date,window )
  mean.spread <- db.handler.return[[1]]
  sd.spread <- db.handler.return[[2]]
  mean.run <- db.handler.return[[3]]
  sd.run <- db.handler.return[[4]]
  spread <- db.handler.return[[5]]
  run <- db.handler.return[[6]]
  spr <- db.handler.return[[7]]
  c1close<-db.handler.return[[8]]
  c2close<-db.handler.return[[9]]
  beta<-db.handler.return[[10]]
  reason <- ""
  if (abs(spread) > mean.spread + tolerance.spread * sd.spread || abs(run) > mean.run + tolerance.run * sd.run) {
    res <- beta
    if (spread > 0) c2close <- -c2close
    if(spread < 0 ) c1close <- -c1close
  }
  else res <- 0
  if (abs(spread) > mean.spread + tolerance.spread * sd.spread) reason <- "OPENED SPREAD"
  if (abs(run) > mean.run + tolerance.run * sd.run) reason <- "OPENED RUNS"
  return(list(res,c1close,c2close,beta,spread, reason))
}