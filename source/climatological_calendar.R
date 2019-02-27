#=============================================================================#
# Name   : climatological_calendar
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Create a climalogical calendar by seconds
# URL    : 
#=============================================================================#
climatological_calendar <- function(){
  years  <- 1:10
  months <- 1:12
  days   <- 1:30
  
  dates <- NULL
  for(i in years){
    for(j in months){
      for(k in days){
        for(l in 1:2){ # Esto permite que el conteo se inicie al medio dia
          fecha_day <- c(i,j,k,l)
          dates <- rbind(dates, fecha_day)
        }
      }
    }
  }
  
  # Number of days in seconds
  days_in_seconds <- seq(from = 0, to = 311040000-60, by = 86400/2)
  dates <- cbind(dates, days_in_seconds)
  dates <- subset(dates, dates[,4] == 1) 
  
  dates <- dates[,c(1,2,3,5)]
  colnames(dates) <- c('Year', 'Month', 'Day', 'Seconds')
  rownames(dates) <- NULL
  return(dates)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#