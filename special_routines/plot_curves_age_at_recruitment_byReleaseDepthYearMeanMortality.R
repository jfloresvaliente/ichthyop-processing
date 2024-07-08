#=============================================================================#
# Name   : plot_curves_age_at_recruitment_byReleaseDepthYearMeanMortality
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Plot curves of percentage of particles at recruited age
# URL    : 
#=============================================================================#
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj_shape_pecq/case1_kx1.6/'
days    <- 91 # Step time
ymax    <- 0.004 # Percetage of particles recruited
ymax2   <- 0.4
depth   <- c('0-15', '15-30', '30-45')

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
cols <- c('red', 'green', 'blue')

load(paste0(dirpath, '/results/recruited_age_percentageReleaseDepthYearMeanMortality.Rdata'))
dat <- age_percent; rm(age_percent)
dat2 <- apply(X = dat, MARGIN = 1, FUN = cumsum)

dat[dat == 0] <- NA
dat2[dat2 == 0] <- NA

png(paste0(dirpath, '/results/recruited_age_percentageReleaseDepthYearMeanMortality.png'), width = 850, height = 850, res = 120)
par(mar = c(5,6,1,1))
plot(1, type = 'n', ylim = c(0,ymax), xlim = c(0,days-1), xlab = '', ylab = '', main = '', yaxs = 'i', xaxs = 'i', axes = F)
axis(side = 1, font = 2, lwd = 2, cex.axis = 1.5, lwd.ticks = 2)
axis(side = 2, font = 2, lwd = 2, cex.axis = 1.5, lwd.ticks = 2, las = 2)
mtext(side = 1, line = 3.2, cex = 1.7, font = 2, text = 'Age at recruitment [d]')
mtext(side = 2, line = 4.6, cex = 1.7, font = 2, text = 'Recruitment [%]')
box(lwd = 2)

legend('topright', legend = depth, lty = 1, lwd = 4, bty = 'n', col = cols, ncol = 1,
       title = 'Spawning Depth [m]', text.font = 2, cex = 1.5)

for(i in 1:length(depth)){
  lines(1:(days-1), dat[i,], col = cols[i], lwd = 4)
}
box()
dev.off()


png(paste0(dirpath, '/results/recruited_age_percentageReleaseDepthYearMeanCumSumMortality.png'), width = 850, height = 850, res = 120)
par(mar = c(5,6,1,1))
plot(1, type = 'n', ylim = c(0,ymax2), xlim = c(0,days-1), xlab = '', ylab = '', main = '', yaxs = 'i', xaxs = 'i', axes = F)
axis(side = 1, font = 2, lwd = 2, cex.axis = 1.5, lwd.ticks = 2)
axis(side = 2, font = 2, lwd = 2, cex.axis = 1.5, lwd.ticks = 2, las = 2)
mtext(side = 1, line = 3.2, cex = 1.7, font = 2, text = 'Age at recruitment [d]')
mtext(side = 2, line = 4.6, cex = 1.7, font = 2, text = 'Cumulative Recruitment [%]')
box(lwd = 2)

legend('topleft', legend = depth, lty = 1, lwd = 4, bty = 'n', col = cols, ncol = 1,
       title = 'Spawning Depth [m]', text.font = 2, cex = 1.5)

for(i in 1:length(depth)){
  lines(1:(days-1), dat2[,i], col = cols[i], lwd = 4)
}
box()
dev.off()

#=============================================================================#
# END OF PROGRAM
#=============================================================================#