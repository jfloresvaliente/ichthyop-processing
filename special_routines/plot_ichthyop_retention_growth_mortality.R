#=============================================================================#
# Name   : plot_ichthyop_retention_growth_mortality
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Plot 2x2: Retencion larval, Crecimiento, Crecimiento + Mortalidad
# URL    : 
#=============================================================================#
source('ichthyop_functions.R')
dirpath   <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB/k_x1.6_90days/out/results/'
retention <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/FISICA/out/results_30days/ichthyop_output.csv'
lats      <- seq(from = 2, to = 20, by = 2)
ylab      <- 'Pre-recruitment (%)'
N0        <- 1 # Initial value of the particle affected by mortality
ymax1     <- c(0,60)
ymax2     <- c(0,1.5)
vertical  <- F # if T --> (4 rows x 1 column) if F --> (2 rows x 2 column)
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º-', lats[i] + 2, 'º'))

retention <- read.table(retention, header = T, sep = ';')
dat <- read.table(paste0(dirpath, 'ichthyop_output_mortality.csv'), header = T, sep = ';')

# Convertir N_length y N_constant en porcentajes
dat$N_length   <- (dat$N_length*100)  /(dat$NumberReleased*N0)
dat$N_constant <- (dat$N_constant*100)/(dat$NumberReleased*N0)

names_dat <- matrix(data = c('Month', 'Depth', 'Bathy', 'Zone_name','Recruitprop',
                             'Month', 'Depth', 'Bathy', 'Zone_name','N_constant',
                             'Month', 'Depth', 'Bathy', 'Zone_name','N_length'), nrow = 3, byrow = T, ncol = 5)

list_all <- list()
for(i in 1:3){
  names_col <- names_dat[i,]
  names_ind <- NULL
  for(j in seq_along(names_col)) names_ind <- c(names_ind, which(names(dat) == names_col[j]))
  dat_sub <- dat[, names_ind]
  
  colnames(dat_sub) <- names_dat[1,]
  
  month <- recruitment_month(dat_sub)
  depth <- recruitment_depth(dat_sub)
  bathy <- recruitment_bathy(dat_sub)
  zone  <- recruitment_zone(dat_sub)
  
  list_all[[i]] <- list(month, depth, bathy, zone)
}
rm(month, depth, bathy, zone)

month  <- recruitment_month(retention)
month1 <- list_all[[1]][[1]]
month2 <- list_all[[2]][[1]]
month3 <- list_all[[3]][[1]]

depth  <- recruitment_depth(retention)
depth1 <- list_all[[1]][[2]]
depth2 <- list_all[[2]][[2]]
depth3 <- list_all[[3]][[2]]

bathy  <- recruitment_bathy(retention)
bathy1 <- list_all[[1]][[3]] 
bathy2 <- list_all[[2]][[3]] 
bathy3 <- list_all[[3]][[3]] 

zone   <- recruitment_zone(retention)
zone1  <- list_all[[1]][[4]]
zone2  <- list_all[[2]][[4]]
zone3  <- list_all[[3]][[4]]

# PLOTS
if(vertical == T){
  png(paste0(dirpath, 'plot_ichthyop_retention_growth_mortalityVERTICAL.png'), height = 1450, width = 750, res = 120)
  par(mfrow = c(4,1))
}else{
  png(paste0(dirpath, 'plot_ichthyop_retention_growth_mortality.png'), height = 850, width = 1450, res = 120)
  par(mfrow = c(2,2))
}

#========================= Plot by spawning month =========================#
par(mar = c(3.5,4,.5,4))

plot(1:12, month[,1], type = 'n', ylim = ymax1, axes = F, xlab = '', ylab = '', xlim = c(.5,12.5))

mtext(side = 1, line = 2  , cex = 0.75, font = 2, text = 'Spawning Month')
mtext(side = 2, line = 2.5, cex = 0.75, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1,    font = 2, text = 'a)', adj = 0.025)
axis(side = 1, at = 1:12, labels = 1:12)
axis(side = 2, at = seq(ymax1[1],ymax1[2],10), labels = seq(ymax1[1],ymax1[2],10), las = 2)

lines(1:12-.13,  month[,1], lwd = 3)
points(1:12-.13, month[,1], pch = 16, cex = 1.5)
arrows(1:12-.13, month[,2], 1:12-.13, month[,3], angle = 90, code = 3, length = 0.02)

lines(1:12,  month1[,1], lwd = 3, lty = 2)
points(1:12, month1[,1], pch = 8, cex = 1.5)
arrows(1:12, month1[,2], 1:12, month1[,3], angle = 90, code = 3, length = 0.02, lty = 5)

par(new = T)
plot(1:12, month2[,1], type = 'n', ylim = ymax2, axes = F, xlab = '', ylab = '',xlim = c(.5,12.5))

lines(1:12+.13,  month2[,1], lwd = 3, col = 'red')
points(1:12+.13, month2[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:12+.13, month2[,2], 1:12+.13, month2[,3], angle = 90, code = 3, length = 0.02, lty = 1, col = 'red')

axis(side = 4, at = seq(ymax2[1],ymax2[2],.5), labels = seq(ymax2[1],ymax2[2],.5), las = 2, col.axis='red', col = 'red')
legend('bottomleft', lty = c(1,2,1), lwd = 2, col = c(1,1,2), bty = 'n', pch = c(16,8,8), seg.len = 5, pt.cex = 1.5,
       legend = c('Age criterion','Size criterion','Size criterion + Constant mortality'))

#========================= Plot by spawning latitude =========================#
par(mar = c(3.5,4,.5,4))

plot(1:9, zone[,1], type = 'n', ylim = ymax1, axes = F, xlab = '', ylab = '', xlim = c(.5,9.5))

mtext(side = 1, line = 2  , cex = 0.75, font = 2, text = 'Spawning Latitude')
mtext(side = 2, line = 2.5, cex = 0.75, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1, font = 2, text = 'b)', adj = 0.025)
axis(side = 1, at = 1:9, labels = rep('',9))
axis(side = 2, at = seq(ymax1[1],ymax1[2],10), labels = seq(ymax1[1],ymax1[2],10), las = 2)
text(1:9, -7, labels = latlab, srt = 45, xpd = TRUE, cex = .75)

lines(1:9-.15,  zone[,1], lwd = 3)
points(1:9-.15, zone[,1], pch = 16, cex = 1.5)
arrows(1:9-.15, zone[,2], 1:9-.15, zone[,3], angle = 90, code = 3, length = 0.02)

lines(1:9,  zone1[,1], lwd = 3, lty = 2)
points(1:9, zone1[,1], pch = 8, cex = 1.5)
arrows(1:9, zone1[,2], 1:9, zone1[,3], angle = 90, code = 3, length = 0.02, lty = 5)

par(new = T)
plot(1:9, zone2[,1], type = 'n', ylim = ymax2, axes = F, xlab = '', ylab = '', xlim = c(.5,9.5))

lines(1:9+.15,  zone2[,1], lwd = 3, col = 'red')
points(1:9+.15, zone2[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:9+.15, zone2[,2], 1:9+.15, zone2[,3], angle = 90, code = 3, length = 0.02, col = 'red')

axis(side = 4, at = seq(ymax2[1],ymax2[2],.5), labels = seq(ymax2[1],ymax2[2],.5), las = 2, col.axis='red', col = 'red')

#========================= Plot by spawning depth =========================#
par(mar = c(3.5,4,.5,4))

plot(1:3, depth[,1], type = 'n', ylim = ymax1, axes = F, xlab = '', ylab = '', xlim = c(.5,3.5))

mtext(side = 1, line = 2  , cex = 0.75, font = 2, text = 'Spawning Depth [m]')
mtext(side = 2, line = 2.5, cex = 0.75, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1,    font = 2, text = 'c)', adj = 0.025)
axis(side = 1, at = 1:3, labels = rownames(depth))
axis(side = 2, at = seq(ymax1[1],ymax1[2],10), labels = seq(ymax1[1],ymax1[2],10), las = 2)

lines(1:3-.09,  depth[,1], lwd = 3)
points(1:3-.09, depth[,1], pch = 16, cex = 1.5)
arrows(1:3-.09, depth[,2], 1:3-.09, depth[,3], angle = 90, code = 3, length = 0.02)

lines(1:3,  depth1[,1], lwd = 3, lty = 2)
points(1:3, depth1[,1], pch = 8, cex = 1.5)
arrows(1:3, depth1[,2], 1:3, depth1[,3], angle = 90, code = 3, length = 0.02, lty = 5)

par(new = T)
plot(1:3, depth2[,1], type = 'n', ylim = ymax2, axes = F, xlab = '', ylab = '', xlim = c(.5,3.5))

lines(1:3+.09,  depth2[,1], lwd = 3, col = 'red')
points(1:3+.09, depth2[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:3+.09, depth2[,2], 1:3+.09, depth2[,3], angle = 90, code = 3, length = 0.02, col = 'red')

axis(side = 4, at = seq(ymax2[1],ymax2[2],.5), labels = seq(ymax2[1],ymax2[2],.5), las = 2, col.axis='red', col = 'red')

#========================= Plot by spawning bathymetry =========================#
par(mar = c(3.5,4,.5,4))

plot(1:3, bathy[,1], type = 'n', ylim = ymax1, axes = F, xlab = '', ylab = '', xlim = c(.5,3.5))

mtext(side = 1, line = 2  , cex = 0.75, font = 2, text = 'Spawning Bathymetry [m]')
mtext(side = 2, line = 2.5, cex = 0.75, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1,    font = 2, text = 'd)', adj = 0.025)
axis(side = 1, at = 1:3, labels = rownames(bathy))
axis(side = 2, at = seq(ymax1[1],ymax1[2],10), labels = seq(ymax1[1],ymax1[2],10), las = 2)

lines(1:3-.09,  bathy[,1], lwd = 3)
points(1:3-.09, bathy[,1], pch = 16, cex = 1.5)
arrows(1:3-.09, bathy[,2], 1:3-.09, bathy[,3], angle = 90, code = 3, length = 0.02)

lines(1:3,  bathy1[,1], lwd = 3, lty = 2)
points(1:3, bathy1[,1], pch = 8, cex = 1.5)
arrows(1:3, bathy1[,2], 1:3, bathy1[,3], angle = 90, code = 3, length = 0.02, lty = 5)

par(new = T)
plot(1:3, bathy2[,1], type = 'n', ylim = ymax2, axes = F, xlab = '', ylab = '', xlim = c(.5,3.5))

lines(1:3+.09,  bathy2[,1], lwd = 3, col = 'red')
points(1:3+.09, bathy2[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:3+.09, bathy2[,2], 1:3+.09, bathy2[,3], angle = 90, code = 3, length = 0.02, col = 'red')

axis(side = 4, at = seq(ymax2[1],ymax2[2],.5), labels = seq(ymax2[1],ymax2[2],.5), las = 2, col.axis='red', col = 'red')

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#