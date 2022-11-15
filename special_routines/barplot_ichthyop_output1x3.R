#=============================================================================#
# Name   : barplot_ichthyop_output
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Barplot recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
source('ichthyop_functions.R')

dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj/case1kx0.4oneyear/'
ymax    <- c(0,25)
lats    <- seq(from = 2, to = 20, by = 2)
ylab    <- 'Recruitment (%)'
hlines  <- seq(from = ymax[1], to = ymax[2], by = 5)
years   <- 1:3 # Number of simulation years

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dat <- read.table(paste0(dirpath, '/results/ichthyop_output.csv'), sep = ';', header = T)
dat$Recruitprop[is.na(dat$Recruitprop)] <- 0
year  <- recruitment_year(dat)
month <- recruitment_month(dat)
age   <- recruitment_age(dat)
coast <- recruitment_behavior(dat)
eps   <- recruitment_eps(dat)
temp  <- recruitment_temp(dat)
area  <- recruitment_area(dat)
depth <- recruitment_depth(dat)
bathy <- recruitment_bathy(dat)
zone  <- recruitment_zone(dat)

yticks1 <- hlines

png(filename = paste0(dirpath, '/results/ichthyop_output.png'), height = 500, width = 1350, res = 120)
par(mfrow = c(1,3), mar = c(5,4,2,1))

# # Plot by year
# yearlab <- levels(factor(dat$Year))
# # yearlab <- NULL
# # for(i in 1:length(years)) yearlab <- c(yearlab, paste0('Y', years[i]))
# 
# yearplot <- barplot(year[,1], ylim = ymax, axes = F, names.arg = F)
# abline(h = hlines, lty = 3, lwd = .05)
# arrows(yearplot, year[,2], yearplot, year[,3], angle = 90, code = 3, length = 0.05)
# mtext(side = 1, line = 2.3, cex = 1, font = 2, text = 'Spawning Year')
# mtext(side = 2, line = 2.5, cex = 1, font = 2, text = ylab)
# axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yearplot, labels = yearlab, lty = 0)
# axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)

# Plot by month
monthplot <- barplot(month[,1], ylim = ymax, axes = F, names.arg = F)
abline(h = hlines, lty = 3, lwd = .05)
arrows(monthplot, month[,2], monthplot, month[,3], angle = 90, code = 3, length = 0.05)
mtext(side = 1, line = 2.7, cex = 1, font = 2, text = 'Spawning Month')
# mtext(side = 2, line = 2.5, cex = 1, font = 2, text = ylab)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.1, font = 2, at = monthplot, labels = rownames(month), lty = 0)
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)

# Plot by latitudinal zone
latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º-', lats[i] + 2, 'º'))

zoneplot <- barplot(zone[,1], ylim = ymax, names.arg = F, axisnames = FALSE, axes = F)
abline(h = hlines, lty = 3, lwd = .05)
arrows(zoneplot, zone[,2], zoneplot, zone[,3], angle = 90, code = 3, length = 0.05)
mtext(side = 1, line = 2.7, cex = 1, font = 2, text = 'Spawning Latitude')
# mtext(side = 2, line = 2.5, cex = 1, font = 2, text = ylab)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = 1:9, labels = rep('',9), lty = 0)
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)
text(zoneplot, -ymax[2]/20, labels = latlab, srt = 20, xpd = TRUE, cex = 1, font = 2)

# Plot by release depth
depthplot <- barplot(depth[,1], ylim = ymax, axes = F, names.arg = F)
abline(h = hlines, lty = 3, lwd = .05)
arrows(depthplot, depth[,2], depthplot, depth[,3], angle = 90, code = 3, length = 0.05)
mtext(side = 1, line = 2.7, cex = 1, font = 2, text = 'Spawning Depth [m]')
mtext(side = 2, line = 2.5, cex = 1, font = 2, text = ylab)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = depthplot, labels = rownames(depth), lty = 0)
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)

# # Plot by release bathymetry
# bathyplot <- barplot(bathy[,1], ylim = ymax, axes = F, names.arg = F)
# abline(h = hlines, lty = 3, lwd = .05)
# arrows(bathyplot, bathy[,2], bathyplot, bathy[,3], angle = 90, code = 3, length = 0.05)
# mtext(side = 1, line = 2.3, cex = 1, font = 2, text = 'Spawning Bathymetry [m]')
# # mtext(side = 2, line = 2.5, cex = 1, font = 2, text = ylab)
# axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = bathyplot, labels = rownames(bathy), lty = 0)
# axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)

# # Plot by coastline behaviours
# coastplot <- barplot(coast[,1], ylim = ymax, axes = F, names.arg = F)
# abline(h = hlines, lty = 3, lwd = .05)
# arrows(coastplot, coast[,2], coastplot, coast[,3], angle = 90, code = 3, length = 0.05)
# mtext(side = 1, line = 2.3, cex = 1, font = 2, text = 'Coastline Behavior')
# # mtext(side = 2, line = 2.5, cex = 1, font = 2, text = ylab)
# axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = coastplot, labels = rownames(coast), lty = 0)
# axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)

# #Plot by release area
# areaplot <- barplot(area[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 3, lwd = .05)
# arrows(areaplot, area[,2], areaplot, area[,3], angle = 90, code = 3, length = 0.05)

dev.off()
rm(latlab)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#