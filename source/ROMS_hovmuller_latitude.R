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
  
  #============ ============ Arguments ============ ============#
  
  # Create the polygon mask in 4D from the ROMS mask
  mask4D <- matrix(data = NA, nrow = dim(mask)[1], ncol = dim(mask)[2])
  
  for(i in 1:dim(xy)[1]) mask4D[ xy[i,1] , xy[i,2] ] <- 1
  
  mask4D <- rep(as.vector(mask4D), time = length(ver_lev) * time_step)
  mask4D <- array(data = mask4D, dim = c(dim(mask)[1], dim(mask)[2], length(ver_lev), time_step))
  
  # Calculate the time series vs depth
  hovmuller <- NULL
  for(year in years[1]:years[2]){print(paste('Year : ', year))
    for(month in months[1]:months[2]){
      
      matfile <- paste0(dirpath, namevar, 'Y', year, 'M', month,'.mat')
      print(matfile)
      vari <- readMat(matfile)$newvar
      
      # if SIMU INTERANUAL EL MES 12 TIENE UN PASO MAS DE TIEMPO
      if(month == 12) vari <- vari[1:time_step,,,]
      
      # Convert to the classical dimension as R reads the ncdf [lon, lat, depth, time].
      vari2 <- array(data = NA, dim = rev(dim(vari)))
      for(i in 1:dim(vari)[1]){
        for(j in 1:dim(vari)[2]){
          subvari <- t(vari[i,j,,])
          vari2[,,j,i] <- subvari
        }
      }
      
      vari <- vari2; rm(vari2)
      
      # If you want to calculate the functional response
      if(!is.null(k_x)) vari <- vari / (vari + k_x)
      
      # Multiplicar por la mascara en 4D con los indices de la zona
      vari <- vari * mask4D
      
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