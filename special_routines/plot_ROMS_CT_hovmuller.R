#=============================================================================#
# Name   : plot_ROMS_CT_hovmuller
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')

dirpath   <- 'C:/Users/jflores/Documents/ICHTHYOP/rsodi1/'
sufijo    <- 'release_zone'
nlevels   <- 64 # Number of levels in the color palette
sp        <- 'encrasicolus' # species name.

#===== Config for temp var =====#
namevar  <- 'TEMP'
zlim     <- c(0.0, 2.3)
isolines <- seq(zlim[1], zlim[2], 0.3) # Isolines to be plotted
caption  <- 'Correction factor'

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
Rdata    <- paste0(dirpath, sufijo, '/', sufijo,'_', namevar,'_hovmuller.Rdata')
load(Rdata)

lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels) # Levels for the color palette

# kelvin degree
T_K <- 273.15

x <- hovmuller$x
y <- hovmuller$y
z <- hovmuller$z + T_K

if(sp == 'encrasicolus'){
  # Engraulis encrasicolus
  T_ref  <- 16 + T_K
  T_A    <- 9800
  
  # Calculate TC for Case 1
  #===== CURVA 1 =====#
  # Parameters
  T_L  <- 6        # C Lower temp boundary
  T_H  <- 21       # C Upper temp boundary
  T_AL <- 20000    # K Arrh. temp for lower boundary
  T_AH <- 190000/2 # K Arrh. temp for upper boundary
  
  # Transforming Celsius to Kelvin boundary temperatures
  T_L   <- T_L + T_K
  T_H   <- T_H + T_K
  
  # Calculate the temperature correction factor.
  s_A  <- exp(T_A/T_ref - T_A/z)  # Arrhenius factor
  
  # 5-parameter correction factor
  if(T_L > T_ref || T_H < T_ref){
    warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')
  }
  
  s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/z - T_AL/T_L))
  s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/z))
  TC_5      <- s_A * ((z <= T_ref) * s_L_ratio + (z > T_ref) * s_H_ratio)
  
  png_name <- paste0(dirpath, sufijo, '/', sufijo, '_', namevar, 'CTcase1', sp, '_hovmuller.png')
  
  png(filename = png_name, width = 1850, height = 750, res = 120)
  par(mar = c(5, 5, 3.5, 3.5))
  filled.contour(x = x, y = y, z = TC_5, zlim = zlim,
                 # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
                 col = tim.colors(length(lev)-1),
                 levels = lev,
                 xlab = '', ylab = '',
                 plot.axes = {
                   contour(x = x, y = y, z = TC_5, levels = isolines, labels = isolines, add = T, lwd = 2, labcex = 1)
                   axis(side = 1, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                   axis(side = 2, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = seq(from = range(y)[1], to = range(y)[2], by = 10))
                   box(lwd = 2)
                 },
                 key.axes = axis(4, isolines, font = 2, lwd.ticks = 2, cex.axis = 1.5)
  )
  mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.45, text = 'Years')
  mtext(side = 2, line = 3.8, font = 2, cex = 1.5, text = 'Depth [m]')
  mtext(side = 3, line = 0.2, font = 2, cex = 2.5, adj = 0.00, text = caption)
  
  dev.off()
  print(range(TC_5))
  
  # Calculate TC for Case 2
  #===== CURVA 2 =====#
  # Parameters
  T_L  <- 6        # C Lower temp boundary
  T_H  <- 24       # C Upper temp boundary
  T_AL <- 20000    # K Arrh. temp for lower boundary
  T_AH <- 190000*3 # K Arrh. temp for upper boundary
  
  # Transforming Celsius to Kelvin boundary temperatures
  T_L   <- T_L + T_K
  T_H   <- T_H + T_K
  
  # Calculate the temperature correction factor.
  s_A  <- exp(T_A/T_ref - T_A/z)  # Arrhenius factor
  
  # 5-parameter correction factor
  if(T_L > T_ref || T_H < T_ref){
    warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')
  }
  
  s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/z - T_AL/T_L))
  s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/z))
  TC_5      <- s_A * ((z <= T_ref) * s_L_ratio + (z > T_ref) * s_H_ratio)
  
  png_name <- paste0(dirpath, sufijo, '/', sufijo, '_', namevar, 'CTcase2', sp, '_hovmuller.png')
  
  png(filename = png_name, width = 1850, height = 750, res = 120)
  par(mar = c(5, 5, 3.5, 3.5))
  filled.contour(x = x, y = y, z = TC_5, zlim = zlim,
                 # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
                 col = tim.colors(length(lev)-1),
                 levels = lev,
                 xlab = '', ylab = '',
                 plot.axes = {
                   contour(x = x, y = y, z = TC_5, levels = isolines, labels = isolines, add = T, lwd = 2, labcex = 1)
                   axis(side = 1, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                   axis(side = 2, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = seq(from = range(y)[1], to = range(y)[2], by = 10))
                   box(lwd = 2)
                 },
                 key.axes = axis(4, isolines, font = 2, lwd.ticks = 2, cex.axis = 1.5)
  )
  mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.45, text = 'Years')
  mtext(side = 2, line = 3.8, font = 2, cex = 1.5, text = 'Depth [m]')
  mtext(side = 3, line = 0.2, font = 2, cex = 2.5, adj = 0.00, text = caption)
  
  dev.off()
  print(range(TC_5))
}

if(sp == 'ringens'){
  # Engraulis encrasicolus
  T_ref  <- 20 + T_K
  T_A    <- 9576
  
  # Calculate TC for Case 1
  #===== CURVA 1 =====#
  # Parameters
  T_L  <- 6        # C Lower temp boundary
  T_H  <- 21       # C Upper temp boundary
  T_AL <- 20000    # K Arrh. temp for lower boundary
  T_AH <- 190000/2 # K Arrh. temp for upper boundary
  
  # Transforming Celsius to Kelvin boundary temperatures
  T_L   <- T_L + T_K
  T_H   <- T_H + T_K
  
  # Calculate the temperature correction factor.
  s_A  <- exp(T_A/T_ref - T_A/z)  # Arrhenius factor
  
  # 5-parameter correction factor
  if(T_L > T_ref || T_H < T_ref){
    warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')
  }
  
  s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/z - T_AL/T_L))
  s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/z))
  TC_5      <- s_A * ((z <= T_ref) * s_L_ratio + (z > T_ref) * s_H_ratio)
  
  png_name <- paste0(dirpath, sufijo, '/', sufijo, '_', namevar, 'CTcase1', sp, '_hovmuller.png')
  
  png(filename = png_name, width = 1850, height = 750, res = 120)
  par(mar = c(5, 5, 3.5, 3.5))
  filled.contour(x = x, y = y, z = TC_5, zlim = zlim,
                 # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
                 col = tim.colors(length(lev)-1),
                 levels = lev,
                 xlab = '', ylab = '',
                 plot.axes = {
                   contour(x = x, y = y, z = TC_5, levels = isolines, labels = isolines, add = T, lwd = 2, labcex = 1)
                   axis(side = 1, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                   axis(side = 2, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = seq(from = range(y)[1], to = range(y)[2], by = 10))
                   box(lwd = 2)
                 },
                 key.axes = axis(4, isolines, font = 2, lwd.ticks = 2, cex.axis = 1.5)
  )
  mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.45, text = 'Years')
  mtext(side = 2, line = 3.8, font = 2, cex = 1.5, text = 'Depth [m]')
  mtext(side = 3, line = 0.2, font = 2, cex = 2.5, adj = 0.00, text = caption)
  
  dev.off()
  print(range(TC_5))
  
  # Calculate TC for Case 2
  #===== CURVA 2 =====#
  # Parameters
  T_L  <- 6        # C Lower temp boundary
  T_H  <- 24       # C Upper temp boundary
  T_AL <- 20000    # K Arrh. temp for lower boundary
  T_AH <- 190000*3 # K Arrh. temp for upper boundary
  
  # Transforming Celsius to Kelvin boundary temperatures
  T_L   <- T_L + T_K
  T_H   <- T_H + T_K
  
  # Calculate the temperature correction factor.
  s_A  <- exp(T_A/T_ref - T_A/z)  # Arrhenius factor
  
  # 5-parameter correction factor
  if(T_L > T_ref || T_H < T_ref){
    warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')
  }
  
  s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/z - T_AL/T_L))
  s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/z))
  TC_5      <- s_A * ((z <= T_ref) * s_L_ratio + (z > T_ref) * s_H_ratio)
  
  png_name <- paste0(dirpath, sufijo, '/', sufijo, '_', namevar, 'CTcase2', sp, '_hovmuller.png')
  
  png(filename = png_name, width = 1850, height = 750, res = 120)
  par(mar = c(5, 5, 3.5, 3.5))
  filled.contour(x = x, y = y, z = TC_5, zlim = zlim,
                 # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
                 col = tim.colors(length(lev)-1),
                 levels = lev,
                 xlab = '', ylab = '',
                 plot.axes = {
                   contour(x = x, y = y, z = TC_5, levels = isolines, labels = isolines, add = T, lwd = 2, labcex = 1)
                   axis(side = 1, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                   axis(side = 2, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = seq(from = range(y)[1], to = range(y)[2], by = 10))
                   box(lwd = 2)
                 },
                 key.axes = axis(4, isolines, font = 2, lwd.ticks = 2, cex.axis = 1.5)
  )
  mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.45, text = 'Years')
  mtext(side = 2, line = 3.8, font = 2, cex = 1.5, text = 'Depth [m]')
  mtext(side = 3, line = 0.2, font = 2, cex = 2.5, adj = 0.00, text = caption)
  
  dev.off()
  print(range(TC_5))
}
# hcl.pals() # Funcion para listar la paleta de color disponible
#=============================================================================#
# END OF PROGRAM
#=============================================================================#