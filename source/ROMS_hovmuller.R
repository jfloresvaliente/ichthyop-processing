#=============================================================================#
# Name   : ROMS_hovmuller
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
ROMS_hovmuller <- function(
  dirpath    = dirpath
  ,namevar   = namevar
  ,mask      = mask
  ,xy        = xy
  ,ver_lev   = ver_lev
  ,time_step = time_step
  ,years     = years
  ,months    = months
  ){
  #============ ============ Arguments ============ ============#
  
  # dirpath   = Directory path which contains series interpolated ROMS files (.mat)
  # namevar   = ROMS variable name
  # mask      = ROMS mask matrix with zeros (0 = land) and ones (1 = water)
  # xy        = 2 column matrix with the X and Y indices of the area to calculate the Hovmuller
  # ver_lev   = vertical levels (in Z) of ROMS
  # time_step = ROMS file time step in days
  # years     = Years between which Hovmuller is to be calculated
  # months    = Months between which Hovmuller is to be calculated

  #============ ============ Arguments ============ ============#
  library(R.matlab)
  
  # Calculate the time series vs depth
  hovmuller <- NULL
  for(year in years[1]:years[2]){
    for(month in months[1]:months[2]){
      
      print(paste('Year : ', year))
      
      matfile <- paste0(dirpath, namevar, 'Y', year, 'M', month,'.Rdata')
      print(matfile)
      load(matfile)

      # Mean horizontally and leaving values for depth and time
      vari <- apply(vari, c(3,4), mean, na.rm = T)
      
      # Add to time series
      hovmuller <- cbind(hovmuller, vari); rm(vari)
    }
  }
  
  hovmuller <- t(hovmuller)
  hovmuller <- hovmuller[,c(length(ver_lev):1)]
  assign(x = 'hovmuller', value = hovmuller, envir = .GlobalEnv)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#