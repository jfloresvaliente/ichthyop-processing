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
    ,k_x       = k_x
    ,z_depth   = -10
    ,lat       = lat
    ,lat_lim   = c(-20,-2)
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
  # rowcut = cortar filas para evitar la sobre carga de datos
  
  #============ ============ Arguments ============ ============#

  # Calculate the time series vs latitude
  hovmuller <- NULL
  for(year in years[1]:years[2]){print(paste('Year : ', year))
    for(month in months[1]:months[2]){
      
      matfile <- paste0(dirpath, namevar, 'Y', year, 'M', month,'.Rdata')
      print(matfile)
      load(matfile)
      
      # If you want to calculate the functional response
      if(!is.null(k_x)) vari <- vari / (vari + k_x)
      
      # Extraer solo hasta la profundidad deseada
      vari <- vari[,,1:which(ver_lev == z_depth),]
      
      # Extraer por cada indice de xy
      lati <- NULL
      lati_valor <- NULL
      for(i in 1:dim(xy)[1]){
        sub_lati <- vari[xy[i,1], xy[i,2],,]
        lati <- rbind(lati, apply(sub_lati, 2, mean, na.rm = T))

        lati_valor <- c(lati_valor, lat[xy[i,1], xy[i,2]])
      }

      # Extraer por latitudes
      lat_in <- seq(lat_lim[1], lat_lim[2], 2)
      lat_on <- lat_in + 2
      
      lati_mat <- NULL
      for(i in 1:(length(lat_in)-1)){
        lat_index <- which(lati_valor > lat_in[i] & lati_valor <= lat_on[i])
        sub_lati <- apply(X = lati[lat_index,], MARGIN = 2, FUN = mean, na.rm = T)
        lati_mat <- rbind(lati_mat, sub_lati)
      }
      
      vari <- lati_mat; rm(lati_mat)

      # Agregar a la serie de tiempo
      hovmuller <- cbind(hovmuller, vari)
    }
  }
  
  hovmuller <- t(hovmuller)
  assign(x = 'hovmuller', value = hovmuller, envir = .GlobalEnv)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#