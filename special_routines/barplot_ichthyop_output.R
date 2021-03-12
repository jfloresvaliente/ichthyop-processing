#=============================================================================#
# Name   : barplot_ichthyop_output
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Compute recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')
source('ichthyop_functions.R')

dirpath  <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/FISICA/out/results_30days/'
ymax     <- 50
lats     <- seq(from = 2, to = 20, by = 2)
hlines   <- seq(0,ymax,10)
years    <- seq(1:3) # Number of simulation years
ylab     <- 'Pre-recruitment (%)'

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dat <- read.table(paste0(dirpath, 'ichthyop_output.csv'), sep = ';', header = T)
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

png(filename = paste0(dirpath, '/ichthyop_output.png'), height = 850, width = 1250, res = 120)
par(mfrow = c(2,3), mar = c(4,3,2,1))

# Plot by year
yearlab <- NULL
for(i in 1:length(years)) yearlab <- c(yearlab, paste0('Y', years[i]))

yearplot <- barplot(year[,1], ylim = c(0, ymax), axes = F, names.arg = yearlab)
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(yearplot, year[,2], yearplot, year[,3], angle = 90, code = 3, length = 0.05)
mtext(side = 1, line = 2 , cex = 0.75, font = 2, text = 'Spawning Year')
mtext(side = 2, line = 2 , cex = 0.65, font = 2, text = ylab)
mtext(side = 3, line = -2, cex = 0.85, font = 2, text = 'a)', adj = 0.025)

# Plot by month
monthplot <- barplot(month[,1], ylim = c(0, ymax), axes = F)
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(monthplot, month[,2], monthplot, month[,3], angle = 90, code = 3, length = 0.05)
mtext(side = 1, line = 2 , cex = 0.75, font = 2, text = 'Spawning Month')
mtext(side = 3, line = -2, cex = 0.85, font = 2, text = 'b)', adj = 0.025)

# Plot by latitudinal zone
latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º-', lats[i] + 2, 'º'))

zoneplot <- barplot(zone[,1], ylim = c(0, ymax), names.arg = F, axisnames = FALSE, axes = F)
text(zoneplot, par('usr')[3], labels = latlab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = .85)
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(zoneplot, zone[,2], zoneplot, zone[,3], angle = 90, code = 3, length = 0.05)
mtext(side = 1, line = 2 , cex = 0.75, font = 2, text = 'Spawning Latitude')
mtext(side = 3, line = -2, cex = 0.85, font = 2, text = 'c)', adj = 0.025)

# Plot by release depth
depthplot <- barplot(depth[,1], ylim = c(0, ymax), axes = F, names.arg = rownames(depth))
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(depthplot, depth[,2], depthplot, depth[,3], angle = 90, code = 3, length = 0.05)
mtext(side = 1, line = 2 , cex = 0.75, font = 2, text = 'Spawning Depth [m]')
mtext(side = 2, line = 2 , cex = 0.65, font = 2, text = ylab)
mtext(side = 3, line = -2, cex = 0.85, font = 2, text = 'd)', adj = 0.025)

# Plot by release bathymetry
bathyplot <- barplot(bathy[,1], ylim = c(0, ymax), axes = F)
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(bathyplot, bathy[,2], bathyplot, bathy[,3], angle = 90, code = 3, length = 0.05)
mtext(side = 1, line = 2 , cex = 0.75, font = 2, text = 'Spawning Bathymetry [m]')
mtext(side = 3, line = -2, cex = 0.85, font = 2, text = 'e)', adj = 0.025)

# #Plot by release area
# areaplot <- barplot(area[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 3, lwd = .05)
# arrows(areaplot, area[,2], areaplot, area[,3], angle = 90, code = 3, length = 0.05)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#