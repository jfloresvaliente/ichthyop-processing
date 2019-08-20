#=============================================================================#
# Name   : recruitment_year
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Calculate the mean, the lower and upper limits of the error bars
#          (confidence interval or standard error) for each release year
# URL    : 
#=============================================================================#
recruitment_year = function(dataset, a = 0.05){
  
  #============ ============ Arguments ============ ============#
  
  # dataset = dataframe with recruitment information
  # with the following format and name of columns
  # [NumberReleased, NumberRecruited, ReleaseArea, Year, Day, Depth, ...
  #  Age, Coast_Behavior, Temp_min, name_file, t_x, Recruitprop ]
  
  # a = confidence interval; if a == NULL, the standard error is calculated
  
  #============ ============ Arguments ============ ============#
  
  #Internal function to calculate error bars
  error_bar <- function(x){
    # x = vector o matrix with data to evaluate
    
    if(!is.null(a)){
      n  <- length(x)
      m  <- mean(x, na.rm = T)
      s  <- sd(x)
      tt <- -qt(a/2,n-1)
      ee <- sd(x)/sqrt(n)  # standard error
      e  <- tt*ee          # error range
      d  <- e/m            # relative error, says that the confidence interval is a percentage of the value
      li <- m-e            # lower limit
      ls <- m+e            # upper limit
      stat <- c(m, li, ls)
    }else{
      n  <- length(x)
      m  <- mean(x, na.rm = T)
      s  <- sd(x)
      tt <- -qt(a/2,n-1)
      ee <- sd(x)/sqrt(n)  # standard error
      # e  <- tt*ee        # error range
      # d  <- e/m          # relative error, says that the confidence interval is a percentage of the value
      li <- m-ee           # lower limit
      ls <- m+ee           # upper limit
      stat <- c(m, li, ls)
    }
    return(stat)
  }
  
  # Get the name of the factors (for string factors)
  fact     <- levels(factor(dataset$Year))
  fact_ini <- NULL
  for(i in 1:length(fact)){
    fact_ini <- c(fact_ini, strsplit(x = fact[i], split = '-')[[1]][1])
  }
  fact     <- fact[order(as.numeric(fact_ini))]
  
  # Get mean and error bars
  errors <- NULL
  for(i in fact){
    facsub <- subset(dataset, dataset$Year == i)
    facsub <- facsub$Recruitprop
    err    <- error_bar(x = facsub)
    errors <- rbind(errors, err)
  }
  colnames(errors) <- c('mean', 'emin', 'emax')
  rownames(errors) <- fact
  
  return(errors)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#