#=============================================================================#
# Name   : plot_curves_age_at_recruitment_byReleaseDepth
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Plot curves of percentage of particles at recruited age
# URL    : 
#=============================================================================#
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEBf1/k_x0/out/results/'
days    <- 91 # Step time
ymax    <- 9 # Percetage of particles recruited
xmax    <- 90  # Days at recruitment
depth   <- c('0-15', '15-30', '30-45')

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
cols <- rep(c('red', 'orange', 'blue', 'green'), each = 3)
line_type <- rep(1:3, times = 4)
meses <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

png(paste0(dirpath, 'recruited_age_percentageByReleaseDepth', '.png'), width = 750, height = 1250, res = 120)
par(mfrow = c(3,1))
for(j in 1:length(depth)){
  load(paste0(dirpath, 'recruited_age_percentage', depth[j], '.Rdata'))
  par(mar = c(5,4,1,1))
  plot(1, type = 'n', ylim = c(0,ymax), xlim = c(0,xmax), xlab = '', ylab = '', main = '', yaxs = 'i', xaxs = 'i', axes = F)
  axis(side = 1, font = 2)
  axis(side = 2, font = 2, las = 2)
  mtext(side = 1, line = 2.5, font = 2, text = 'Age at recruitment [d]')
  mtext(side = 2, line = 2.5, font = 2, text = 'Recruitment [%]')
  legend('topright', legend = meses, lty = line_type, bty = 'n', col = cols, ncol = 4)
  legend('topleft' , legend = paste('Release Depth', depth[j], 'm'), bty = 'n', text.font = 2, cex = 1.25)
  for(i in 1:12){
    lines(0:(days-1), recruited_age_percentage[,i], col = cols[i], lwd = 2, lty = line_type[i])
  }
  box()
}
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#