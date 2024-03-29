#=============================================================================#
# Name   : recruitment_bathy
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Calculate the mean, the lower and upper limits of the error bars
#          (confidence interval or standard error) for each release bathymetry
# URL    : 
#=============================================================================#
recruitment_bathy = function(dataset, a = 0.05){
  
  #============ ============ Arguments ============ ============#
  
  # dataset = Dataframe of ichthyop output information with the following format and name of columns
  # ['NumberReleased', 'NumberRecruited', 'ReleaseArea', 'Year', 'Month', 'Eps', ...
  # 'Age', 'Coast_Behavior', 'Temp_min', 'Name_file', 'Zone_name', 'Depth' ...
  # 'Bathy', 'Particles', 'Recruitprop']
  # a = confidence interval; if a == NULL, the standard error is calculated
  
  #============ ============ Arguments ============ ============#
  
  #Internal function to calculate error bars
  error_bar <- function(x){
    # x = vector or matrix with data to evaluate
    
    if(!is.null(a)){
      n  <- length(x)
      m  <- mean(x, na.rm = T)
      tt <- -qt(p = a/2, df = n-1)
      ee <- sd(x)/sqrt(n)  # standard error
      e  <- tt*ee          # error range
      d  <- e/m            # relative error, says that the confidence interval is a percentage of the value
      li <- m-e            # lower limit
      ls <- m+e            # upper limit
      stat <- c(m, li, ls)
    }else{
      n  <- length(x)
      m  <- mean(x, na.rm = T)
      tt <- -qt(p = a/2, df = n-1)
      ee <- sd(x)/sqrt(n)  # standard error
      li <- m-ee           # lower limit
      ls <- m+ee           # upper limit
      stat <- c(m, li, ls)
    }
    return(stat)
  }

  # Get the name of the factors (for string factors)
  fact     <- levels(factor(dataset$ReleaseBathy))
  fact_ini <- NULL
  for(i in 1:length(fact)){
    fact_ini <- c(fact_ini, strsplit(x = fact[i], split = '-')[[1]][1])
  }
  fact     <- fact[order(as.numeric(fact_ini))]
  
  # Get mean and error bars
  errors <- NULL
  for(i in fact){
    facsub <- subset(dataset, dataset$ReleaseBathy == i)
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