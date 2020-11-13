#=============================================================================#
# Name   : plot_hovmuller_zlev_getlineROMS
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('source/ichthyop_libraries.R')
dirpath   <- 'E:/ROMS_SILUMATIONS/10kmparent/interpolated/'
xy        <- read.table(paste0(dirpath, 'getline_rowcol_index.txt'))
ver_lev   <- as.vector(read.table(paste0(dirpath, 'depth.txt'), header = T))[,1]
depth_lim <- range(ver_lev)   # Latitude extension of the area 
nlevels   <- 25               # Number of levels in the color palette

# # Config for V var
# namevar  <- 'v'
# zlim     <- c(-0.076, 0.076)
# isolines <- round(seq(zlim[1], zlim[2], 0.01), 2) # Isolines to be plotted
# caption  <- expression('Velocity V [m/s]')

# # Config for U var
# namevar  <- 'u'
# zlim     <- c(-0.15, 0.15)
# isolines <- round(seq(zlim[1], zlim[2], 0.02), 2) # Isolines to be plotted
# caption  <- expression('Velocity U [m/s]')

# # Config for temp var
# namevar  <- 'temp'
# zlim     <- c(13, 22)
# isolines <- round(seq(zlim[1], zlim[2], 1), 2) # Isolines to be plotted
# caption  <- expression('Temperature [ºC]')

# # Config for O2 var
# namevar  <- 'O2'
# zlim     <- c(0, 255)
# isolines <- round(seq(zlim[1], zlim[2], 20), 2) # Isolines to be plotted
# caption  <- expression('Oxygen [umol L-1]')

# Config for MESO var
namevar  <- 'MESO'
zlim     <- c(0, 7)
isolines <- round(seq(zlim[1], zlim[2], 1), 2) # Isolines to be plotted
caption  <- expression('Meso-zoo [umol C L-1]')

#======= Do not change anything from here=======#

month_mean <- NULL
for(month in 1:12){
  
  matfile <- paste0(dirpath, namevar,'M', month,'.mat')
  vari <- readMat(matfile)$newvar
  
  vari2 <- array(data = NA, dim = rev(dim(vari)))
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

png(filename = paste0(dirpath, toupper(namevar),'hovmuller_getline.png'), width = 850, height = 850, res = 120)
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
mtext(text = caption, side = 3, line = .5, adj = 0, font = 2, cex = 1.1)
dev.off()
# hcl.pals() # Funcion para listar la paleta de color disponible
#=============================================================================#
# END OF PROGRAM
#=============================================================================#