#=============================================================================#
# Name   : ROMSmat2NA
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
ROMSmat2NA <- function(
    dirpath    = dirpath
    ,namevar   = namevar
    ,mask      = mask
    ,xy        = xy
    ,ver_lev   = ver_lev
    ,time_step = time_step
    ,years     = years
    ,months    = months
    ,sufijo    = sufijo
){
  library(R.matlab)
  library(stringr)
  
  #============ ============ Arguments ============ ============#
  
  # dirpath   = Directory path which contains series interpolated ROMS files (.mat)
  # namevar   = ROMS variable name
  # mask      = ROMS mask matrix with zeros (0 = land) and ones (1 = water)
  # xy        = 2 column matrix with the X and Y indices of the area to calculate the Hovmuller
  # ver_lev   = vertical levels (in Z) of ROMS
  # time_step = ROMS file time step in days
  # years     = Years between which Hovmuller is to be calculated
  # months    = Months between which Hovmuller is to be calculated
  # sufijo = 

  #============ ============ Arguments ============ ============#
  
  # Create the polygon mask in 4D from the ROMS mask
  mask4D <- matrix(data = NA, nrow = dim(mask)[1], ncol = dim(mask)[2])
  
  for(i in 1:dim(xy)[1]) mask4D[ xy[i,1] , xy[i,2] ] <- 1
  
  mask4D <- rep(as.vector(mask4D), times = length(ver_lev) * time_step)
  mask4D <- array(data = mask4D, dim = c(dim(mask)[1], dim(mask)[2], length(ver_lev), time_step))
  
  for(year in years[1]:years[2]){print(paste('Year : ', year))
    for(month in months[1]:months[2]){
      
      matfile <- paste0(dirpath, namevar, 'Y', year, 'M', month,'.mat')
      vari <- readMat(matfile)$newvar

      # If this is an inter-annual simulation, month 12 (December) must have an additional time step.
      if(month == 12) vari <- vari[1:time_step,,,]
      
      # Convert to the classical dimension as R reads the ncdf [lon, lat, depth, time].
      vari2 <- array(data = NA, dim = rev(dim(vari)))
      for(i in 1:dim(vari)[1]){
        for(j in 1:dim(vari)[2]){
          vari2[,,j,i] <- t(vari[i,j,,])
        }
      }
      
      # Multiply by 4D mask with zone indices
      vari  <- vari2 * mask4D; rm(vari2)
      Rdata <- paste0(dirpath, sufijo, '/', namevar, 'Y', year, 'M', month,'.Rdata')
      print(paste0('saving .....', Rdata))
      save(vari, file = Rdata)
    }
  }
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#