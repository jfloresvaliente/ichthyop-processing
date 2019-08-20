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
  
  # Number of days in hours
  horas <- seq(from = 0, to = (length(years)*360*24*60*60), by = 3600)[-c(1)]
  dias  <- rep(1:30, each = 24, length.out = length(horas))
  meses <- rep(1:12, each = 30*24, length.out = length(horas))
  ans   <- rep(1:10, each = 12*30*24, length.out = length(horas))

  dates <- cbind(ans, meses, dias, horas)
  colnames(dates) <- c('Year', 'Month', 'Day', 'Seconds')
  rownames(dates) <- NULL
  return(dates)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#