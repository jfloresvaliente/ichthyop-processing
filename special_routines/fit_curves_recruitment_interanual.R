#=============================================================================#
# Name   : fit_curves_recruitment_interanual
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Barplot associated to spawning depth & Lines associated to spawning bathymetry.
# URL    : 
#=============================================================================#
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj/case2kx0.2/results/'
ymax    <- 40
ylab <- 'Recruitment (%)'
years   <- 1980:2000 # Number of simulation years

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dat   <- read.table(file = paste0(dirpath,'ichthyop_output.csv'), header = T, sep = ';')
# dat$Recruitprop <- dat$N_constantprop
depth <- tapply(dat$Recruitprop, list(dat$Year, dat$ReleaseDepth), mean, na.rm = T)
bathy <- tapply(dat$Recruitprop, list(dat$Year, dat$ReleaseBathy), mean, na.rm = T)

cols_legend <- c('grey5', 'grey50', 'grey80')

cols <- c('grey5', 'grey50', 'grey80')
border <- c('white', 'white', 'white')

# cols <- c('white', 'white', 'grey80')
# border <- c('white', 'white', 'white')

# Nivel 1
png(filename = paste0(dirpath, 'barplot_curves00-15.png'), height = 750, width = 1050, res = 120)
par(mar=c(6,4,1,.5), xpd=TRUE)
plot1 <- barplot(t(depth[,1]), beside = T, ylim = c(0,ymax), axes = F, col = cols[1], names.arg = rep('',length(years)), border = border)
x <- as.vector(plot1)
y <- as.vector(t(depth[,1]))
xy <- data.frame(x,y)
fit1 <- lm(y ~ poly(x, 3, raw=TRUE), data = xy)
x_axis <- seq(range(x)[1], range(x)[2], .1) 
# lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='black', lwd = 4, lty = 3)

axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.3, font = 2, at = plot1, labels = rep('', times = length(years)))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.5, font = 2, las = 2)
mtext(side = 2, text = ylab, lwd = 2, font = 2, line = 2.8, cex = 1.5)
text(plot1, -ymax/20, labels = years, srt = 20, xpd = TRUE, cex = 1, font = 2)

# mtext(side = 1, lwd = 2, font = 2, line = 2.9, cex = 1.2, adj = 0.03, text = 'Spawning Depth [m] (Histograms)')
# legend('bottomleft', inset=c(0.05,-0.23), legend = colnames(depth), bty = 'n', horiz = T, title.adj = 0, text.font = 2,
#        fill = cols_legend, title = '', cex = 1.1)

dev.off()


# Nivel 2
png(filename = paste0(dirpath, 'barplot_curves15-30.png'), height = 750, width = 1050, res = 120)
par(mar=c(6,4,1,.5), xpd=TRUE)
plot1 <- barplot(t(depth[,2]), beside = T, ylim = c(0,ymax), axes = F, col = cols[2], names.arg = rep('',length(years)), border = border)
x <- as.vector(plot1)
y <- as.vector(t(depth[,2]))
xy <- data.frame(x,y)
fit1 <- lm(y ~ poly(x, 3, raw=TRUE), data = xy)
x_axis <- seq(range(x)[1], range(x)[2], .1) 
# lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='black', lwd = 4, lty = 3)

axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.3, font = 2, at = plot1, labels = rep('',length(years)))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.5, font = 2, las = 2)
mtext(side = 2, text = ylab, lwd = 2, font = 2, line = 2.8, cex = 1.5)
text(plot1, -ymax/20, labels = years, srt = 20, xpd = TRUE, cex = 1, font = 2)

# mtext(side = 1, lwd = 2, font = 2, line = 2.9, cex = 1.2, adj = 0.03, text = 'Spawning Depth [m] (Histograms)')
# legend('bottomleft', inset=c(0.05,-0.23), legend = colnames(depth), bty = 'n', horiz = T, title.adj = 0, text.font = 2,
#        fill = cols_legend, title = '', cex = 1.1)

dev.off()

# Nivel 3
png(filename = paste0(dirpath, 'barplot_curves30-45.png'), height = 750, width = 1050, res = 120)
par(mar=c(6,4,1,.5), xpd=TRUE)
plot1 <- barplot(t(depth[,3]), beside = T, ylim = c(0,ymax), axes = F, col = cols[3], names.arg = rep('',length(years)), border = border)
x <- as.vector(plot1)
y <- as.vector(t(depth[,3]))
xy <- data.frame(x,y)
fit1 <- lm(y ~ poly(x, 3, raw=TRUE), data = xy)
x_axis <- seq(range(x)[1], range(x)[2], .1) 
# lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='black', lwd = 4, lty = 3)

axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.3, font = 2, at = plot1, labels = rep('',length(years)))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.5, font = 2, las = 2)
mtext(side = 2, text = ylab, lwd = 2, font = 2, line = 2.8, cex = 1.5)
text(plot1, -ymax/20, labels = years, srt = 20, xpd = TRUE, cex = 1, font = 2)

# mtext(side = 1, lwd = 2, font = 2, line = 2.9, cex = 1.2, adj = 0.03, text = 'Spawning Depth [m] (Histograms)')
mtext(side = 1, lwd = 2, font = 2, line = 2.9, cex = 1.2, adj = 0.03, text = 'Release Depth [m] (Histograms)')
legend('bottomleft', inset=c(0.05,-0.23), legend = colnames(depth), bty = 'n', horiz = T, title.adj = 0, text.font = 2,
       fill = cols_legend, title = '', cex = 1.1)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#