#=============================================================================#
# Name   : plot_curves_age_at_recruitment
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Plot curves of the percentage of particles at age at recruitment.
# URL    : 
#=============================================================================#
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB/k_x1.6_90days/out/results/'
ymax    <- 4.5 # Percetage of particles recruited
xmax    <- 90  # Days at recruitment

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

load(paste0(dirpath, 'recruited_age_percentage.Rdata'))

cols <- rep(c('red', 'orange', 'blue', 'green'), each = 3)
line_type <- rep(1:3, times = 4)
meses <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Agu','Sep','Oct','Nov','Dec')

png(paste0(dirpath, 'recruited_age_percentage.png'), width = 850, height = 850, res = 120)
par(mar = c(5,4,1,1))
plot(1, type = 'n', ylim = c(0,ymax), xlim = c(0,xmax), xlab = '', ylab = '', main = '', yaxs = 'i', xaxs = 'i', axes = F)
axis(side = 1, font = 2)
axis(side = 2, font = 2, las = 2)
mtext(side = 1, line = 2.5, font = 2, text = 'Age at recruitment [d]')
mtext(side = 2, line = 2.5, font = 2, text = 'Recruited particles [%]')
legend('topright', legend = meses, lty = line_type, bty = 'n', col = cols, ncol = 4)
for(i in 1:12){
  lines(0:(xmax), recruited_age_percentage[,i], col = cols[i], lwd = 2, lty = line_type[i])
}
box()
dev.off()
m <- as.numeric(apply(X = recruited_age_percentage, MARGIN = 2, FUN = which.max))
m
mean(m[1:3])
mean(m[7:9])
#=============================================================================#
# END OF PROGRAM
#=============================================================================#