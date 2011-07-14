## This function executes trading exit commands depending on spread or runs standard deviation
FindExitSignal <- function (company.1,   # first company in pair
                            company.2,   # secondcompany in pair
                            date,       # current date when likeliness of trading is examined
                            window = 365, ## number of days before the current day, whose data are considered for analyzing current status
                            exit.tolerance.spread = 4, # coefficient of sigma, standard deviation for spread
                            exit.tolerance.run = 4)   # coefficient of sigma, standard deviation for runs
  {
  db.handler.return <- DatabaseHandler (company.1, company.2, date, window)
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
  res<-ifelse (abs(spread) > mean.spread + exit.tolerance.spread * sd.spread || 
    abs(run) > mean.run + exit.tolerance.run * sd.run ||  abs(spread) <= mean.spread + sd.spread 
    || abs(run) <= mean.run + sd.run, 0,1)
  if (abs(spread) > mean.spread + exit.tolerance.spread * sd.spread || 
    abs(spread) <= mean.spread + sd.spread) reason <- "CLOSED SPREAD"
  if(abs(run) > mean.run + exit.tolerance.run * sd.run || abs(run) <= mean.run + sd.run) reason <- "CLOSED RUNS"
  return(list(res, c1close, c2close, beta, spread, reason))
}