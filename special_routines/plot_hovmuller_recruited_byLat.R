#=============================================================================#
# Name   : plot_hovmuller_recruited_byLat
# Author : 
# Date   : 
# Version:
# Aim    : Calcula el reclutamiento por latitud, ejm, 0.25º o 2º, etc
# URL    : 
#=============================================================================#
library(fields)

dirpath       <- 'E:/ICHTHYOP/10kmparent/FISICA/out/results/'
latilim       <- c(-20, -2)    # Latitude extension of the area
lat_div       <- .25
computeattime <- 30 # Tiempo  en dias

#------------- Do not change anything after here -------------#
lat_ini <- seq(latilim[1], latilim[2], lat_div)
lat_out <- lat_ini + 2

m <- NULL
n <- NULL
for(j in 1:12){
  Rdata <- paste0(dirpath,'trajectoriesM',j,'.Rdata')
  load(Rdata)
  print(Rdata)
  dat <- trajectories; rm(trajectories)
  
  dat_ini <- subset(dat, dat$Timer == 1)

  a <- NULL
  b <- NULL 
  for(i in 1:(length(lat_ini)-1)){
    lat_sub <- subset(dat_ini, dat_ini$Lat >= lat_ini[i] & dat_ini$Lat < lat_out[i])
    
    released  <- dim(lat_sub)[1]
    recruited <- dim(subset(dat, dat$Drifter %in% lat_sub$Drifter & dat$Timer == computeattime+2 & dat$IfRecruited ==1))[1]
    
    a <- c(a, released)
    b <- c(b, recruited)
  }
  m <- cbind(m, a)
  n <- cbind(n, b)
}

percent <- (n/m)*100


# Plot Hovmuller
zlim     <- 50            # Retention rate interval to be plotted
nlevels  <- 25            # Number of levels in the color palette
isolines <- seq(0,zlim,5) # Isolines to be plotted

z <- t(percent)
x <- 1:12
y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])
lev <- seq(from = 0, to = zlim, length.out = nlevels)

png(filename = paste0(dirpath, 'hovmullerRecruitedNewCalculation.png'), width = 850, height = 850, res = 120)
filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1), levels = lev,
               xlab = 'Months', ylab = 'Latitude',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
                 axis(1, 1:12)
                 axis(2, seq(latilim[1],latilim[2], by = 2))
               })
dev.off()

png(filename = paste0(dirpath, 'hovmullerRecruitedImagePlot.png'), width = 850, height = 850, res = 120)
image.plot(x,y,z, axes = F, xlab = 'Months', ylab = 'Latitude', zlim = c(0, zlim))
axis(1)
axis(2)
box()
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#