#=============================================================================#
# Name   : plot_ichthyop_retention_growth_mortality_noshelf
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Plot 2x2: Larval Retention, Growth + Mortality, Growth + Mortality (no shelf)
# URL    : 
#=============================================================================#
source('ichthyop_functions.R')
dirpath   <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB/k_x1.6/out/'
# retention <- paste0(dirpath, '/results/ichthyop_output.csv')
lats      <- seq(from = 2, to = 20, by = 2)
ylab      <- 'Recruitment (%)'
ymax1     <- c(0,60)
ymax2     <- c(0,5)
vertical  <- F # if T --> (4 rows x 1 column) if F --> (2 rows x 2 column)
legend_text <- c('Size criterion','Size criterion + Constant mortality','Size criterion + Constant mortality (non shelf)')
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

# Generate latitude names
latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º - ', lats[i] + 2, 'º'))

# Larval retention data
dat1 <- read.table(paste0(dirpath, '/results/ichthyop_output2.csv'), header = T, sep = ';')

# Larval recruitment data
dat2 <- read.table(paste0(dirpath, '/results/ichthyop_output2.csv'), header = T, sep = ';')
dat2$Recruitprop <- dat2$N_constantprop

# Larval recruitment data + constant mortality data
dat3 <- read.table(paste0(dirpath, '/results_no_shelf/ichthyop_output2.csv'), header = T, sep = ';')
dat3$Recruitprop <- dat3$N_constantprop

# Get mean values and confidence intervals for each factor.
month1 <- recruitment_month(dat1)
month2 <- recruitment_month(dat2)
month3 <- recruitment_month(dat3)

depth1 <- recruitment_depth(dat1)
depth2 <- recruitment_depth(dat2)
depth3 <- recruitment_depth(dat3)

bathy1 <- recruitment_bathy(dat1)
bathy2 <- recruitment_bathy(dat2) 
bathy3 <- recruitment_bathy(dat3) 

zone1  <- recruitment_zone(dat1)
zone2  <- recruitment_zone(dat2)
zone3  <- recruitment_zone(dat3)

# Configuration of the chart output panel
if(vertical == T){
  png(paste0(dirpath, 'plot_ichthyop_retention_growth_mortality_noshelfVERTICAL.png'), height = 1450, width = 750, res = 120)
  par(mfrow = c(4,1))
}else{
  png(paste0(dirpath, 'plot_ichthyop_retention_growth_mortality_noshelf.png'), height = 850, width = 1450, res = 120)
  par(mfrow = c(2,2))
}

yticks1 <- seq(ymax1[1],ymax1[2],10)
yticks2 <- seq(ymax2[1],ymax2[2],.5)
#========================= Plot by spawning month =========================#
par(mar = c(3.5,4,.5,4))

plot(1:12, month1[,1], type = 'n', ylim = ymax1, axes = F, xlab = '', ylab = '', xlim = c(.5,12.5))

mtext(side = 1, line = 2  , cex = 1, font = 2, text = 'Spawning Month')
mtext(side = 2, line = 2.5, cex = 1, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1,    font = 2, text = 'a)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = 1:12, labels = 1:12)
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)

# dat1 curve
lines(1:12-.13,  month1[,1], lwd = 3, lty = 2)
points(1:12-.13, month1[,1], pch = 8, cex = 1.5)
arrows(1:12-.13, month1[,2], 1:12-.13, month1[,3], angle = 90, code = 3, length = 0.02)

# dat2 curve
par(new = T)
plot(1:12, month2[,1], type = 'n', ylim = ymax2, axes = F, xlab = '', ylab = '',xlim = c(.5,12.5))

lines(1:12,  month2[,1], lwd = 3, lty = 2, col = 'red')
points(1:12, month2[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:12, month2[,2], 1:12, month2[,3], angle = 90, code = 3, length = 0.02, lty = 5, col = 'red')

# dat3 curve
lines(1:12+.13,  month3[,1], lwd = 3, col = 'red')
points(1:12+.13, month3[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:12+.13, month3[,2], 1:12+.13, month3[,3], angle = 90, code = 3, length = 0.02, lty = 1, col = 'red')

axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)
legend('topright', lty = c(2,2,1), lwd = 2, col = c(1,2,2), bty = 'n', pch = c(8,8,8), seg.len = 5, pt.cex = 1.5,
       legend = legend_text, text.font = 2, cex = 0.85)

#========================= Plot by spawning latitude =========================#
par(mar = c(3.5,4,.5,4))

plot(1:9, zone1[,1], type = 'n', ylim = ymax1, axes = F, xlab = '', ylab = '', xlim = c(.5,9.5))

mtext(side = 1, line = 2.1, cex = 1, font = 2, text = 'Spawning Latitude')
mtext(side = 2, line = 2.5, cex = 1, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1, font = 2, text = 'b)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = 1:9, labels = rep('',9))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)
text(1:9, -7, labels = latlab, srt = 20, xpd = TRUE, cex = 1, font = 2)

# dat1 curve
lines(1:9-.15,  zone1[,1], lwd = 3)
points(1:9-.15, zone1[,1], pch = 8, cex = 1.5)
arrows(1:9-.15, zone1[,2], 1:9-.15, zone1[,3], angle = 90, code = 3, length = 0.02)

# dat2 curve
par(new = T)
plot(1:9, zone2[,1], type = 'n', ylim = ymax2, axes = F, xlab = '', ylab = '', xlim = c(.5,9.5))

lines(1:9,  zone2[,1], lwd = 3, lty = 2, col = 'red')
points(1:9, zone2[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:9, zone2[,2], 1:9, zone2[,3], angle = 90, code = 3, length = 0.02, lty = 5, col = 'red')

# dat3 curve
lines(1:9+.15,  zone3[,1], lwd = 3, col = 'red')
points(1:9+.15, zone3[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:9+.15, zone3[,2], 1:9+.15, zone3[,3], angle = 90, code = 3, length = 0.02, col = 'red')

axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)

#========================= Plot by spawning depth =========================#
par(mar = c(3.5,4,.5,4))

plot(1:3, depth1[,1], type = 'n', ylim = ymax1, axes = F, xlab = '', ylab = '', xlim = c(.5,3.5))

mtext(side = 1, line = 2  , cex = 1, font = 2, text = 'Spawning Depth [m]')
mtext(side = 2, line = 2.5, cex = 1, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1,    font = 2, text = 'c)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = 1:3, labels = rownames(depth1))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)

# dat1 curve
lines(1:3-.09,  depth1[,1], lwd = 3)
points(1:3-.09, depth1[,1], pch = 8, cex = 1.5)
arrows(1:3-.09, depth1[,2], 1:3-.09, depth1[,3], angle = 90, code = 3, length = 0.02)

# dat2 curve
par(new = T)
plot(1:3, depth2[,1], type = 'n', ylim = ymax2, axes = F, xlab = '', ylab = '', xlim = c(.5,3.5))

lines(1:3,  depth2[,1], lwd = 3, lty = 2, col = 'red')
points(1:3, depth2[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:3, depth2[,2], 1:3, depth2[,3], angle = 90, code = 3, length = 0.02, lty = 5, col = 'red')

# dat3 curve
lines(1:3+.09,  depth3[,1], lwd = 3, col = 'red')
points(1:3+.09, depth3[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:3+.09, depth3[,2], 1:3+.09, depth3[,3], angle = 90, code = 3, length = 0.02, col = 'red')

axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)

#========================= Plot by spawning bathymetry =========================#
par(mar = c(3.5,4,.5,4))

plot(1:3, bathy1[,1], type = 'n', ylim = ymax1, axes = F, xlab = '', ylab = '', xlim = c(.5,3.5))

mtext(side = 1, line = 2  , cex = 1, font = 2, text = 'Spawning Bathymetry [m]')
mtext(side = 2, line = 2.5, cex = 1, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1,    font = 2, text = 'd)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = 1:3, labels = rownames(bathy1))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)

# dat1 curve
lines(1:3-.09,  bathy1[,1], lwd = 3)
points(1:3-.09, bathy1[,1], pch = 8, cex = 1.5)
arrows(1:3-.09, bathy1[,2], 1:3-.09, bathy1[,3], angle = 90, code = 3, length = 0.02)

# dat2 curve
par(new = T)
plot(1:3, bathy2[,1], type = 'n', ylim = ymax2, axes = F, xlab = '', ylab = '', xlim = c(.5,3.5))

lines(1:3,  bathy2[,1], lwd = 3, lty = 2, col = 'red')
points(1:3, bathy2[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:3, bathy2[,2], 1:3, bathy2[,3], angle = 90, code = 3, length = 0.02, lty = 5, col = 'red')

# dat3 curve
lines(1:3+.09,  bathy3[,1], lwd = 3, col = 'red')
points(1:3+.09, bathy3[,1], pch = 8, cex = 1.5, col = 'red')
arrows(1:3+.09, bathy3[,2], 1:3+.09, bathy3[,3], angle = 90, code = 3, length = 0.02, col = 'red')

axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#