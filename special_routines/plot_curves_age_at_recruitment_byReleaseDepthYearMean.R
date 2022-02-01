#=============================================================================#
# Name   : plot_curves_age_at_recruitment_byReleaseDepthYearMean
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Plot curves of percentage of particles at recruited age
# URL    : 
#=============================================================================#
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu7/results_no_shelf/'
days    <- 91 # Step time
ymax    <- 4 # Percetage of particles recruited
depth   <- c('0-15', '15-30', '30-45')

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
cols <- c('red', 'green', 'blue')

load(paste0(dirpath, 'recruited_age_percentageReleaseDepthYearMean.Rdata'))
dat <- age_percent; rm(age_percent)
dat[dat == 0] <- NA

png(paste0(dirpath, 'recruited_age_percentageReleaseDepthYearMean.png'), width = 850, height = 850, res = 120)
par(mar = c(5,5,1,1))
plot(1, type = 'n', ylim = c(0,ymax), xlim = c(0,days-1), xlab = '', ylab = '', main = '', yaxs = 'i', xaxs = 'i', axes = F)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2)
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, las = 2)
mtext(side = 1, line = 2.8, cex = 1.3, font = 2, text = 'Age at recruitment [d]')
mtext(side = 2, line = 2.8, cex = 1.3, font = 2, text = 'Recruitment [%]')
box(lwd = 2)

legend('topright', legend = depth, lty = 1, lwd = 4, bty = 'n', col = cols, ncol = 1,
       title = 'Spawning Depth [m]', text.font = 2, cex = 1.5)

for(i in 1:length(depth)){
  lines(1:(days-1), dat[i,], col = cols[i], lwd = 4)
}
box()
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#