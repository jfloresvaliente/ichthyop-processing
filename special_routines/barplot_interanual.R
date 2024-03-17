#=============================================================================#
# Name   : barplot_interanual
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Barplot recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
source('ichthyop_functions.R')

dirpath <- 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj/out_case2/'
ymax    <- c(0,20)
lats    <- seq(from = 2, to = 20, by = 2)
ylab    <- 'Recruitment (%)'
hlines  <- seq(from = ymax[1], to = ymax[2], by = 5)
# years   <- seq(1980:2000) # Number of simulation years

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dat <- read.table(paste0(dirpath, '/results/ichthyop_output.csv'), sep = ';', header = T)
dat <- subset(dat, dat$Zone_name != 'zone1') # Quitar la zona1 'Golfo de Guayaquil - Ecuador'
dat$Recruitprop[is.na(dat$Recruitprop)] <- 0
year  <- recruitment_year(dat)

yticks1 <- hlines

# Plot de las 3 profundidades juntas
png(filename = paste0(dirpath, '/results/barplot_interanual.png'), height = 450, width = 1550, res = 120)
par(mar = c(5,5,1,1))

# Plot by year
yearlab <- levels(factor(dat$Year))

yearplot <- barplot(year[,1], ylim = ymax, axes = F, names.arg = F)
abline(h = hlines, lty = 3, lwd = .05)
arrows(yearplot, year[,2], yearplot, year[,3], angle = 90, code = 3, length = 0.05)
mtext(side = 1, line = 2.7, cex = 1.5, font = 2, text = 'Spawning Year')
mtext(side = 2, line = 2.7, cex = 1.5, font = 2, text = ylab)

axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yearplot, labels = rep(x = '', times = length(yearlab)), lty = 0)
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)
# text(zoneplot, -ymax[2]/20, labels = latlab, srt = 20, xpd = TRUE, cex = 1, font = 2)
text(yearplot, -ymax[2]/20, labels = yearlab, srt = 20, xpd = TRUE, cex = 1, font = 2)
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#