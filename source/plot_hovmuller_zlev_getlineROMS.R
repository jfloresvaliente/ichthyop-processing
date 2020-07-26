#=============================================================================#
# Name   : plot_hovmuller_zlev_getlineROMS
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
library(R.matlab)
library(fields)
library(vcd)
dirpath   <- 'E:/ROMS_SILUMATIONS/10kmparent/'
xy        <- read.table(paste0(dirpath, 'getline_rowcol_index.txt'))
ver_lev   <- c(-1, -5, -10, -15, -20, -25, -30, -35, -40, -45, -50, -60, -70, -80, -90, -100)
depth_lim <- range(ver_lev)   # Latitude extension of the area 
nlevels   <- 25               # Number of levels in the color palette

# # Config for V var
# namevar <- 'vz'
# zlim    <- c(-0.076, 0.076)
# isolines <- round(seq(zlim[1], zlim[2], 0.01), 2) # Isolines to be plotted

# # Config for U var
# namevar <- 'uz'
# zlim    <- c(-0.15, 0.15)
# isolines <- round(seq(zlim[1], zlim[2], 0.02), 2) # Isolines to be plotted

# # Config for temp var
# namevar <- 'temp'
# zlim    <- c(10, 25)
# isolines <- round(seq(zlim[1], zlim[2], 2), 2) # Isolines to be plotted

# # Config for O2 var
# namevar <- 'O2'
# zlim    <- c(0, 255)
# isolines <- round(seq(zlim[1], zlim[2], 20), 2) # Isolines to be plotted
# title    <- expression('Oxygen [umol L-1]')

# Config for MESO var
namevar  <- 'MESO'
zlim     <- c(0, 5)
isolines <- round(seq(zlim[1], zlim[2], 1), 2) # Isolines to be plotted
title    <- expression('Meso-zoo [umol C L-1]')


# no cambiar desde aqui
month_mean <- NULL
for(month in 1:12){
  matfile <- paste0(dirpath, '/interpolated/',namevar,'M', month,'.mat')
  vari <- readMat(matfile)$newvar; dim(vari)
  vari2 <- array(data = NA, dim = rev(dim(vari))); dim(vari2)
  for(i in 1:dim(vari)[1]){
    for(j in 1:dim(vari)[2]){
      subvari <- t(vari[i,j,,])
      vari2[,,j,i] <- subvari
    }
  }
  vari <- vari2; rm(vari2)
  vari <- apply(vari, c(1,2,3), mean)
  mask <- matrix(data = NA, nrow = dim(vari)[1], ncol = dim(vari)[2])
  
  for(i in 1:dim(xy)[1]){
    mask[ xy[i,1] , xy[i,2] ] <- 1
  }
  
  zlev_mean <- NULL
  for(i in 1:dim(vari)[3]){
    zlev_mean <- c(zlev_mean, mean(vari[,,i] * mask, na.rm = T))
  }
  
  month_mean <- rbind(month_mean, zlev_mean)
  print(matfile)
}

rownames(month_mean) <- 1:12
colnames(month_mean) <- abs(ver_lev)

month_mean <- month_mean[,c(length(ver_lev):1)]
lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels)

x <- 1:12
y <- rev(ver_lev)
# 
png(filename = paste0(dirpath, '/interpolated/', namevar,'hovmuller_getline.png'), width = 850, height = 850, res = 120)
filled.contour(x = x, y = y, z = month_mean, zlim = zlim,
               col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               # col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = 'Months', ylab = 'Depth',
               plot.axes = {
                 contour(x = x, y = y, z = month_mean, levels = isolines, labels = isolines, add = T)
                 axis(1, 1:12)
                 axis(2, seq(depth_lim[1],depth_lim[2], by = 10))
               }
)
mtext(text = title, side = 3, line = .5, adj = 0, font = 2, cex = 1.1)
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#
# hcl.pals() # Funcion para listar la paleta de color disponible