#=============================================================================#
# Name   : ROMS_hovmuller_latitude
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
ROMS_hovmuller_latitude <- function(
    dirpath    = dirpath
    ,namevar   = namevar
    ,mask      = mask
    ,xy        = xy
    ,ver_lev   = ver_lev
    ,time_step = time_step
    ,years     = years
    ,months    = months
    ,z_depth   = -10
    ,lat       = lat
    ,lat_lim   = c(-20,-2)
    ,lat_inter = 2
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
  # z_depth   = depth at which the vertical mean is computed (must be a negative number)
  # lat       = lat_rho of the hydrodynamic model
  # lat_lim   = latitude limit range to be calculated
  # lat_inter = latitudinal interval, if smaller, takes longer to calculate
  
  #============ ============ Arguments ============ ============#
  library(R.matlab)
  
  # Calculate the time series vs latitude
  hovmuller <- NULL
  for(year in years[1]:years[2]){
    for(month in months[1]:months[2]){
      
      print(paste('Year : ', year))
      
      matfile <- paste0(dirpath, namevar, 'Y', year, 'M', month,'.Rdata')
      print(matfile)
      load(matfile)
      
      # Extract only to the target depth
      vari <- vari[,,1:which(ver_lev == z_depth),]
      
      # Extract for each xy index
      lati <- NULL
      lati_valor <- NULL
      for(i in 1:dim(xy)[1]){
        sub_lati <- vari[xy[i,1], xy[i,2],,]
        lati <- rbind(lati, apply(sub_lati, 2, mean, na.rm = T))

        lati_valor <- c(lati_valor, lat[xy[i,1], xy[i,2]])
      }

      # Get mean value by each latitudinal interval.
      lat_in <- seq(from = lat_lim[1], to = lat_lim[2], by = lat_inter)
      lat_on <- lat_in + lat_inter
      
      lati_mat <- NULL
      for(i in 1:(length(lat_in)-1)){
        lat_index <- which(lati_valor > lat_in[i] & lati_valor <= lat_on[i])
        sub_lati <- apply(X = lati[lat_index,], MARGIN = 2, FUN = mean, na.rm = T)
        lati_mat <- rbind(lati_mat, sub_lati)
      }
      
      vari <- lati_mat; rm(lati_mat)

      # Add to time series
      hovmuller <- cbind(hovmuller, vari)
    }
  }
  
  hovmuller <- t(hovmuller)
  assign(x = 'hovmuller', value = hovmuller, envir = .GlobalEnv)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#