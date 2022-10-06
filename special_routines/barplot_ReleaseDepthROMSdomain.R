source('ichthyop_libraries.R')
source('ichthyop_functions.R')

dirpath <- 'C:/Users/jflores/Desktop/'
dat1 <- read.table('C:/Users/jflores/Documents/ICHTHYOP/AnalisisSensibilidad/sen10km/out/ichthyop_output.csv', header = T, sep = ';')
dat1 <- subset(dat1, dat1$TotalParticles == 5000)

dat2 <- read.table('C:/Users/jflores/Documents/ICHTHYOP/AnalisisSensibilidad/sen02km/out/ichthyop_output.csv', header = T, sep = ';')
dat2 <- subset(dat2, dat2$TotalParticles == 5000)

dat3 <- read.table('C:/Users/jflores/Documents/ICHTHYOP/AnalisisSensibilidad/sen02km_new/out/ichthyop_output.csv', header = T, sep = ';')
dat3 <- subset(dat3, dat3$TotalParticles == 5000)

ylab <- 'Retention (%)'

ymax     <- c(0,50)
col_bars <- c('grey10','grey50','grey90')
legend   <- c('D01', 'D02s', 'D02r')

png_name <- paste0(dirpath, 'barplot_ichthyop_comparison3bars.png')
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
arrow_col <- 'blue'
yticks <- seq(ymax[1],ymax[2],10)

coast1 <- tapply(dat1$Recruitprop, list(dat1$Coast_Behavior, dat1$ReleaseBathy), mean, na.rm = T)
coast2 <- tapply(dat2$Recruitprop, list(dat2$Coast_Behavior, dat2$ReleaseBathy), mean, na.rm = T)
coast3 <- tapply(dat3$Recruitprop, list(dat3$Coast_Behavior, dat3$ReleaseBathy), mean, na.rm = T)
# coast  <- rbind(coast1[,1], coast2[,1], coast3[,1])

#========================= PLOT =========================#
png(png_name, height = 650, width = 1250, res = 120)
par(mfrow = c(1,3))

#========================= Plot by coast1 =========================#
par(mar = c(5,4,1,4))
coastplot   <- barplot(coast1, beside = T, xlab='', ylab= '' ,ylim = ymax,
                       axes = T, axisnames = F, col = col_bars, yaxt='n')

mtext(side = 1, line = 3.0, cex = 1.3, font = 2, text = 'Release Bathymetry')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -2 , cex = 1.5, font = 2, text = 'a)', adj = 0.025)
mtext(side = 3, line = -2 , cex = 1.5, font = 2, text = 'D01', adj = 0.5)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = apply(coastplot,2,mean), labels = colnames(coast1))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks, labels = yticks, las = 2)

legend(x = 8.5, y = 40,
       bty = 'n',
       fill = col_bars,
       border = col_bars,
       seg.len = 5,
       pt.cex  = 1.5,
       legend  = rownames(coast1),
       text.font = 2,
       cex = 0.9
)

#========================= Plot by coast2 =========================#
par(mar = c(5,4,1,4))
coastplot   <- barplot(coast2, beside = T, xlab='', ylab= '' ,ylim = ymax,
                       axes = T, axisnames = F, col = col_bars, yaxt='n')

mtext(side = 1, line = 3.0, cex = 1.3, font = 2, text = 'Release Bathymetry')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -2 , cex = 1.5, font = 2, text = 'b)', adj = 0.025)
mtext(side = 3, line = -2 , cex = 1.5, font = 2, text = 'D02s', adj = 0.5)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = apply(coastplot,2,mean), labels = colnames(coast1))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks, labels = yticks, las = 2)

legend(x = 8.5, y = 40,
       bty = 'n',
       fill = col_bars,
       border = col_bars,
       seg.len = 5,
       pt.cex  = 1.5,
       legend  = rownames(coast1),
       text.font = 2,
       cex = 0.9
)

#========================= Plot by coast3 =========================#
par(mar = c(5,4,1,4))
coastplot   <- barplot(coast3, beside = T, xlab='', ylab= '' ,ylim = ymax,
                       axes = T, axisnames = F, col = col_bars, yaxt='n')

mtext(side = 1, line = 3.0, cex = 1.3, font = 2, text = 'Release Bathymetry')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -2 , cex = 1.5, font = 2, text = 'c)', adj = 0.025)
mtext(side = 3, line = -2 , cex = 1.5, font = 2, text = 'D02r', adj = 0.5)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = apply(coastplot,2,mean), labels = colnames(coast1))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks, labels = yticks, las = 2)

legend(x = 8.5, y = 40,
       bty = 'n',
       fill = col_bars,
       border = col_bars,
       seg.len = 5,
       pt.cex  = 1.5,
       legend  = rownames(coast1),
       text.font = 2,
       cex = 0.9
)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#