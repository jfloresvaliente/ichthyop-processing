library(R.matlab)
library(fields)
library(vcd)
dirpath   <- 'C:/Users/jflores/Desktop/'
matfile <- paste0(dirpath, 'imarpe_O2_data.mat')
# xy        <- read.table(paste0(dirpath, 'getzone_rowcol_index.txt'))
# ver_lev   <- c(-1, -5, -10, -15, -20, -25, -30, -35, -40, -45, -50, -60, -70, -80, -90, -100)
# depth_lim <- range(ver_lev)   # Latitude extension of the area 
nlevels   <- 25               # Number of levels in the color palette

vari <- readMat(matfile); dim(vari)

O2 <- vari$mO2; dim(O2)
O2 <- matrix(data = rev(as.vector(O2)), nrow = 13, ncol = 12); dim(O2)
depth <- vari$Zlev

x <- 1:12
y <- rev(depth)
z <- t(O2)

lev <- seq(from = 0, to = 250, length.out = nlevels)
isolines <- seq(0, 250, 20) # Isolines to be plotted
title    <- expression('Oxygen [umol L-1]')

png(filename = paste0(dirpath, '/imarpeO2.png'), width = 850, height = 850, res = 120)
filled.contour(x = x, y = y, z = z, zlim = c(0,250),
               col = hcl.colors(n = length(lev), palette = 'Blue-Red 3'),
               # col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = 'Months', ylab = 'Depth',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
                 axis(1, 1:12)
                 axis(2, seq(-100,10, by = 10))
               }
)
mtext(text = title, side = 3, line = .5, adj = 0, font = 2, cex = 1.1)
dev.off()
