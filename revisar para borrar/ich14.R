#=============================================================================#
# Name   : plot_hovmuller_zlev_getzoneROMS
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
library(R.matlab)
library(fields)
library(vcd)
library(ncdf4)
dirpath   <- 'E:/ROMS_SILUMATIONS/10kmparent/'
xy        <- read.table(paste0(dirpath, 'getline_rowcol_index.txt'))
ver_lev   <- c(-1, -5, -10, -15, -20, -25, -30, -50, -70, -100)
depth_lim <- range(ver_lev)   # Latitude extension of the area 
nlevels   <- 25               # Number of levels in the color palette

# Config for temp var
namevar <- 'temp'
zlim    <- c(10, 25)
isolines <- round(seq(zlim[1], zlim[2], 2), 2) # Isolines to be plotted
title   <- expression('Temperature [Cº]')

# # Config for O2 var
# namevar <- 'O2'
# zlim    <- c(0, 255)
# isolines <- round(seq(zlim[1], zlim[2], 20), 2) # Isolines to be plotted
# title   <- expression('Oxygen [umol L-1]')

# Obtener el promedio anual
year <- array(data = NA, dim = c(261,277,10,12))
for(month in 1:12){
  matfile <- paste0(dirpath, '/interpolated/',namevar,'M', month,'.mat')
  vari <- readMat(matfile)$newvar; dim(vari); print(matfile)
  vari2 <- array(data = NA, dim = rev(dim(vari))); dim(vari2)
  for(i in 1:dim(vari)[1]){
    for(j in 1:dim(vari)[2]){
      subvari <- t(vari[i,j,,])
      vari2[,,j,i] <- subvari
    }
  }
  vari <- vari2; rm(vari2)
  vari <- apply(vari, c(1,2,3), mean)
  year[,,,month] <- vari
}

year <- apply(year, c(1,2,3), mean); dim(year)
# FIN: Obtener el promedio anual

# Obtener promedios por latitud y profundidad
nc <- nc_open(filename = list.files(path = dirpath, pattern = 'nc', full.names = T)[1])
lat <- ncvar_get(nc, 'lat_rho')

xy$lat <- NA
for(i in 1:dim(xy)[1]){
  xy$lat[i] <- lat[xy[i,1] , xy[i,2]]
}

lati <- seq(-20,-2,1)
latiname <- NULL
latimean <- NULL
for(i in 1:(length(lati)-1)){
  subindex <- subset(xy, xy$lat >= lati[i] & xy$lat < (lati[i]+1))
  
  mask <- matrix(data = NA, nrow = dim(year)[1], ncol = dim(year)[2])
  for(j in 1:dim(subindex)[1]){
    mask[ subindex[j,1] , subindex[j,2] ] <- 1
  }
  
  depthmean <- NULL
  for(depth in 1:dim(year)[3]){
    depthmean <- c(depthmean, mean(year[,,depth] * mask, na.rm = T))
  }
  
  latimean <- rbind(latimean, depthmean)
  latiname <- c(latiname, mean(c(lati[i], lati[i]+1)))
}
rownames(latimean) <- latiname
colnames(latimean) <- ver_lev
# FIN: Obtener los indices LON/LAT por cada latitud
latimean <- latimean[,c(length(ver_lev):1)]
latimean <- latimean[c(length(latiname):1),]
lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels)

x <- rev(abs(latiname))
y <- rev(ver_lev)
z <- latimean
filled.contour(x = x, y = y, z = z, zlim = zlim,
               col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               # col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = 'Latitude (Sº)', ylab = 'Depth',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
                 axis(1, x)
                 axis(2, seq(depth_lim[1],depth_lim[2], by = 10))
               }
)
mtext(text = title, side = 3, line = .5, adj = 0, font = 2, cex = 1.1)
