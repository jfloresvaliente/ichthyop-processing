#=============================================================================#
# Name   : forward_backward
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Plot FORWARD and BACKWARD Ichthyop simulation
# URL    : 
#=============================================================================#

library(ncdf4)

nc1 <- 'C:/Users/jflores/Documents/JORGE/colaboradores/OmarVaramiento/roms3d_ichthyop-run202007301307.nc.part'
nc2 <- 'C:/Users/jflores/Documents/JORGE/colaboradores/OmarVaramiento/roms3d_ichthyop-run202007301304.nc.part'

nc1 <- nc_open(nc1)
lon1 <- ncvar_get(nc1, 'lon')
lat1 <- ncvar_get(nc1, 'lat')

nc2 <- nc_open(nc2)
lon2 <- ncvar_get(nc2, 'lon')
lat2 <- ncvar_get(nc2, 'lat')

x11()
plot(lon1, lat1, type = 'n', xlim = c(-85.25, -84.7), xlab = 'lon', ylab = 'lat')
lines(lon1, lat1)
lines(lon2, lat2, col = 'red')
legend('bottomright', legend = c('Forward', 'Backward'), lty = 1, col = c('black', 'red'), bty = 'n', text.col = c('black', 'red'))

#=============================================================================#
# END OF PROGRAM
#=============================================================================#