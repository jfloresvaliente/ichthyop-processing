#=============================================================================#
# Name   : ROMS_hovmuller_CT
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
ROMS_hovmuller_CT <- function(
    dirpath    = dirpath
    ,namevar   = namevar
    ,mask      = mask
    ,xy        = xy
    ,ver_lev   = ver_lev
    ,time_step = time_step
    ,years     = years
    ,months    = months
    ,T_K       = 273.15
    ,T_ref     = T_ref
    ,T_A       = T_A
    ,T_L       = 6
    ,T_H       = 21
    ,T_AL      = 20000
    ,T_AH      = 190000/2
){

  library(R.matlab)
  
  #============ ============ Arguments ============ ============#
  
  # dirpath   = Directory path which contains series interpolated ROMS files (.mat)
  # namevar   = ROMS variable name
  # mask      = ROMS mask matrix with zeros (0 = land) and ones (1 = water)
  # xy        = 2 column matrix with the X and Y indices of the area to calculate the Hovmuller
  # ver_lev   = vertical levels (in Z) of ROMS
  # time_step = ROMS file time step in days
  # years     = Years between which Hovmuller is to be calculated
  # months    = Months between which Hovmuller is to be calculated
  # T_K       = kelvin degrees
  # T_ref     = T_ref
  # T_A       = Arrhenius temperature (species-specific)
  # T_L       = Lower temperature boundary (Celsius degrees)
  # T_H       = Upper temperature boundary (Celsius degrees)
  # T_AL      = Arrhenius temperature for lower boundary (Kelvin degrees)
  # T_AH      = Arrhenius temperature for upper boundary (Kelvin degrees)

  #============ ============ Arguments ============ ============#
  
  # Transforming Celsius to Kelvin boundary temperatures
  T_ref <- T_ref + T_K
  T_L   <- T_L + T_K
  T_H   <- T_H + T_K

  # # Create the polygon mask in 4D from the ROMS mask
  # mask4D <- matrix(data = NA, nrow = dim(mask)[1], ncol = dim(mask)[2])
  # 
  # for(i in 1:dim(xy)[1]) mask4D[ xy[i,1] , xy[i,2] ] <- 1
  # 
  # mask4D <- rep(as.vector(mask4D), time = length(ver_lev) * time_step)
  # mask4D <- array(data = mask4D, dim = c(dim(mask)[1], dim(mask)[2], length(ver_lev), time_step))
  
  # Calculate the time series vs depth
  hovmuller <- NULL
  for(year in years[1]:years[2]){print(paste('Year : ', year))
    for(month in months[1]:months[2]){
      
      matfile <- paste0(dirpath, namevar, 'Y', year, 'M', month,'.Rdata')
      print(matfile)
      load(matfile)
      
      vari <- vari + T_K
      
      # matfile <- paste0(dirpath, namevar, 'Y', year, 'M', month,'.mat')
      # print(matfile)
      # vari <- readMat(matfile)$newvar + T_K
      
      # Calculate the temperature correction factor.
      s_A  <- exp(T_A/T_ref - T_A/vari)  # Arrhenius factor
      
      # 5-parameter correction factor
      if(T_L > T_ref || T_H < T_ref){
        warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')
      }
      
      s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/vari - T_AL/T_L))
      s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/vari))
      TC_5      <- s_A * ((vari <= T_ref) * s_L_ratio + (vari > T_ref) * s_H_ratio)
      vari      <- TC_5
      
      # # If this is an inter-annual simulation, month 12 (December) must have an additional time step.
      # if(month == 12) vari <- vari[1:time_step,,,]
      
      # # Convert to the classical dimension as R reads the ncdf [lon, lat, depth, time].
      # vari2 <- array(data = NA, dim = rev(dim(vari)))
      # for(i in 1:dim(vari)[1]){
      #   for(j in 1:dim(vari)[2]){
      #     subvari <- t(vari[i,j,,])
      #     vari2[,,j,i] <- subvari
      #   }
      # }
      # 
      # vari <- vari2; rm(vari2)
      # 
      # # Multiply by 4D mask with zone indices
      # vari <- vari * mask4D
      
      # Mean horizontally and leaving values for depth and time
      vari <- apply(vari, c(3,4), mean, na.rm = T)
      
      # Add to time series
      hovmuller <- cbind(hovmuller, vari)
    }
  }
  
  hovmuller <- t(hovmuller)
  hovmuller <- hovmuller[,c(length(ver_lev):1)]
  assign(x = 'hovmuller', value = hovmuller, envir = .GlobalEnv)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#