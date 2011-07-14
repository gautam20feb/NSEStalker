library(RMySQL)
library(stats)
library("lmtest")
# library(betareg)
library(RMySQL)
library(moments)
library (timeDate)

#**********************************************************************#
## Add all these codes to working directory before running
source("~/projects/NSEStalker/mark_to_mark_sources/db_handler.R")       
 # handles the database to return mean, variance, spread, beta and runs of period given
source("~/projects/NSEStalker/mark_to_mark_sources/find_entry_signal.R") 
 # Takes arguments for entering into trade and returns pair's residual and spread
source("~/projects/NSEStalker/mark_to_mark_sources/find_exit_signal.R")
 # executes trading exit commands
source("~/projects/NSEStalker/mark_to_mark_sources/RunsTest.R")
 # does runs test on the given series
     
#***********************************************************************#
mylog <- file("Trading_log.csv", "w") 
 ##<< For logging the entry and exit positions and timstamps for each pair
nearest_expiry_date <- read.csv("~/projects/NSEStalker/mark_to_mark_sources/nearest_expiry_date.csv")
 ##<< Get a list of all working days with respective nearest expiry dates.Cuurently we are interested only in working days. Should  
 ##<<incorporate other data too in near future.

pair.current <- read.csv("~/projects/NSEStalker/mark_to_mark_sources/pair.csv", header=T)
 ##<< Gets the list of all the 23 pairs. Change in the this csv, if you want to change any company in 23 pairs.

pair.current$value <-0 ##<< Initiliasing 
pair.current <- data.frame(pair.current, 0)
names(pair.current)<-c("SNo", "company.1", "company.2", "value", "exposure")
pair.yesterday <- pair.current      
 # at the end of day, dynamical assignment of current day to the previous one so that yesteday's data is not lost to caluculate certain parameters
                 
date <- "2010-04-01" ##<< Start date of the simulation
duration <- 60 ##<< Duration
dates<-date ##<< A vector that is made to contain a list of all dates in the above duration.
for(j in seq(duration)){
  dates<-c(dates,as.character(as.Date(date)+j))
}
final <- data.frame()  ##<< Initialising a dataframe that records everything pertaining to every pair everyday.
transaction <- data.frame() ##<< Intiialising a dataframe that records all data pertaining just to those days when trasactions were made and when a position is held in any dataframe
summary <- data.frame(0,0,0,0,0,0,0,0,0,0,0,0)
names(summary) <- c("TIMESTAMP", "COMPANY1", "COMPANY2", "TRANSACTION","BETA", "POSITION", "CLOSE1", "CLOSE2", "EXPOSURE","REALIZED", "UNREALIZED", "REASON")
##<< summary is a temporary moving dataframe containing daily data that is used to add cumulatively to 'final' and 'transaction' databases
for (i in 1:nrow(pair.current)) {
  t<-0 ##<< Temporary index used to cumulatively add all daily unrealized profits until the day they are realized
  pair.yesterday$value[i]<-0
  for (date in dates)  {
    if (length(which(nearest_expiry_date$Day == date)) != 0){ ##<< If it is a working day
      
      summary$COMPANY1 <- as.character(pair.current$company.1[i]) 
      summary$COMPANY2 <- as.character(pair.current$company.2[i])
      summary$TIMESTAMP <- date
      
      if(pair.yesterday$value[i] != 0){ ##<< If we already have a position
        
        value <- FindExitSignal (as.character(pair.current$company.1[i]), as.character(pair.current$company.2[i]), date)
        if (value[[1]] == 1) value[[1]] <- pair.yesterday$value[i]
        pair.current$value[i] <- value[[1]] ##<< Giving output to current value
        
        summary$BETA <- pair.yesterday$value[i]
        summary$CLOSE1 <- value[[2]]
        summary$CLOSE2 <- value[[3]]
        summary$REASON <- value[[6]]
        summary$TRANSACTION <- "NO TRANSACTION"
        summary$POSITION <- 1
        
        if (pair.current$value[i] == 0) { ##<< If the position is now changed to zero, that is Exit signal actually indicated to exit
          summary$TRANSACTION <- "CLOSED"
          summary$POSITION <- 0
          cat(date,as.character(pair.current$company.1[i]),as.character(pair.current$company.2[i]),"Position Closed","Beta =",pair.yesterday$value[i],"c2=",value[[3]],"c1=",value[[2]],"(m*x)=",value[[3]]*pair.yesterday$value[i],"\n",file = mylog, sep = ",")
        }
      }
      else if (pair.yesterday$value[i] == 0) { ##<< If we don't have a position
        
        value <- FindEntrySignal (as.character(pair.current$company.1[i]), as.character(pair.current$company.2[i]), date)
        pair.current$value[i] <- value[[1]] ##<< Updating current value with output from Entry signal
        
        summary$BETA <- value[[4]]
        summary$CLOSE1 <- value[[2]]
        summary$CLOSE2 <- value[[3]]
        summary$REASON <- value[[6]]
        summary$TRANSACTION <- "NO TRANSACTION"
        summary$POSITION <- 0
        
        if (pair.current$value[i] != 0) { ##<< If the position is now 1 i.e. Entry signal actually indicated enter a position...
          summary$TRANSACTION <- "OPENED"
          summary$POSITION <- 1
          cat(date,as.character(pair.current$company.1[i]),as.character(pair.current$company.2[i]),"Position Opened","Beta=",value[[4]],"c2=",value[[3]],"c1=",value[[2]],"(m*x)=",value[[3]]*value[[4]],"\n",file = mylog, sep = ",")
        }
      }
      
      summary$REALIZED<-0 
      summary$UNREALIZED <- 0
	##<< Intialiizing Relaized and UNrealized profits      

      if(summary$POSITION == 1 || summary$TRANSACTION == "CLOSED") {
        if (summary$TRANSACTION == "OPENED") {
          t1 <- sign(summary$CLOSE1) 
          t2 <- sign(summary$CLOSE2)
	    ##<< In this particular case, just passing the sign of the Closing prices on the day of opening a position to all other days on 		which the position is held.

        }
        else {
          summary$CLOSE1 <- abs(summary$CLOSE1)*t1
          summary$CLOSE2 <- abs(summary$CLOSE2)*t2
        }
      }
      
      summary$EXPOSURE <- round((summary$BETA*summary$CLOSE2 + summary$CLOSE1), digits =3 )
      
      if ((summary$TRANSACTION == "NO TRANSACTION"  && summary$POSITION == 1)|| summary$TRANSACTION=="CLOSED" ) {
        summary$UNREALIZED <- (pair.yesterday$exposure[i] - summary$EXPOSURE)
      }
      t<-t+summary$UNREALIZED ##<< Accumulating all unrealized profit until the day of closing the position
      if (summary$TRANSACTION=="CLOSED") {
        summary$REALIZED <- t ##<< Dumping the accumulated unrealized profits to realized profits on the day of closing
        t<- 0 ##<< Intilizing the temp varibale back to zero for acccumulation
      }  
      pair.current$exposure <- summary$EXPOSURE
      pair.yesterday <- pair.current ##<< Passing current values to yesterday for the next loop
        
      if(summary$POSITION == 1 || summary$TRANSACTION == "CLOSED") transaction <- rbind(transaction, summary)
      final <- rbind(final, summary)
    }
  }
}
close(mylog)

final1 <-final[with(final, order(TIMESTAMP)), ]
transaction.day <- transaction[with(transaction, order(TIMESTAMP)), ]
##<< Ordering with timestamp

trading.day <- data.frame() 
cum.unrealized <- 0
cum.realized <- 0

## Loop for caluculting cumulative unrrealized and realized profits
for (date in unique(transaction.day$TIMESTAMP)){
  unrealized <- 0
  realized <- 0
  count <- which(transaction.day$TIMESTAMP == date)
  for (i in count) {
    unrealized <- unrealized + transaction.day$UNREALIZED[i]
    realized <- realized + transaction.day$REALIZED[i]
  }
  cum.unrealized <- cum.unrealized + unrealized
  cum.realized <- cum.realized + realized
  temp <- data.frame (date, unrealized, realized, cum.unrealized, cum.realized)
  names (temp) <- c("TIMESTAMP", "UNREALIZED", "REALIZED", "CUM_UNREALIZED", "CUM_REALIZED")
  trading.day <- rbind(trading.day, temp)
}  

#***********************************************************************#
 ## Results and Plots
par (mfrow = c(2,1))
plot (as.Date(trading.day$TIMESTAMP) , trading.day$CUM_UNREALIZED, type = 'l', xlab = "TIMESTAMP", ylab = "UNREALIZED PROFIT")
plot (as.Date(trading.day$TIMESTAMP) , trading.day$CUM_REALIZED, type = 'l', xlab = "TIMESTAMP", ylab = "REALIZED PROFIT")
sum (transaction$REALIZED)
sharpe <- (mean(trading.day$UNREALIZED) / sd (trading.day$UNREALIZED))*16
sum (transaction$REALIZED)
sharpe
