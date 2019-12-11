source('source/recruitment_age.R')
source('source/recruitment_area.R')
source('source/recruitment_bathy.R')
source('source/recruitment_behavior.R')
source('source/recruitment_day.R')
source('source/recruitment_depth.R')
source('source/recruitment_eps.R')
source('source/recruitment_temp.R')
source('source/recruitment_year.R')
source('source/recruitment_zone.R')

dat1 <- read.table('E:/ICHTHYOP/peru10km/ParticleNumber/out/5000/results/ichthyop_output.csv', header = T, sep = ';')
dat2 <- read.table('E:/ICHTHYOP/peru10km/ParticleNumber/out/10000/results/ichthyop_output.csv', header = T, sep = ';')
dat3 <- read.table('E:/ICHTHYOP/peru10km/ParticleNumber/out/20000/results/ichthyop_output.csv', header = T, sep = ';')

# legend_text = c('15 000', '30 000')
legend_text = c('15 000', '30 000', '60 000')
ylim = c(0,60)
ylab = 'Local Retention (%)'
# col_bars <- c('grey20','grey80')
col_bars <- c('grey10','grey50','grey90')
legpos <- 'topright'
years <- c('Y1')
zonename <- c('6º-8º','8º-10º','10º-12º','12º-14º')
legencex <- 1

png('C:/Users/jflores/Desktop/ichthyop_results.png', width = 1250, height = 850, res = 120)
par(mfrow = c(2,3))

# Barplot by day
day1 <- recruitment_day(dat1)
day2 <- recruitment_day(dat2)
day3 <- recruitment_day(dat3)
day <- cbind(day1[,1], day2[,1], day3[,1])
# day <- cbind(day1[,1], day2[,1])

dayplot <- barplot(t(day), beside = T, ylim = ylim, col = col_bars)
arrows(dayplot[1,], day1[,2], dayplot[1,], day1[,3], angle = 90, code = 3, length = 0.02)
arrows(dayplot[2,], day2[,2], dayplot[2,], day2[,3], angle = 90, code = 3, length = 0.02)
arrows(dayplot[3,], day3[,2], dayplot[3,], day3[,3], angle = 90, code = 3, length = 0.02)
mtext(text = ylab, side = 2, line = 2)
mtext(text = 'Release Month', side = 1, line = 2.5)
legend(legpos, legend = legend_text, bty = 'n', fill = col_bars, cex = legencex)

# Barplot by depth
depth1 <- recruitment_depth(dat1)
depth2 <- recruitment_depth(dat2)
depth3 <- recruitment_depth(dat2)
depth <- cbind(depth1[,1], depth2[,1], depth3[,1])
# depth <- cbind(depth1[,1], depth2[,1])

depthplot <- barplot(t(depth), beside = T, ylim = ylim, col = col_bars)
arrows(depthplot[1,], depth1[,2], depthplot[1,], depth1[,3], angle = 90, code = 3, length = 0.02)
arrows(depthplot[2,], depth2[,2], depthplot[2,], depth2[,3], angle = 90, code = 3, length = 0.02)
arrows(depthplot[3,], depth3[,2], depthplot[3,], depth3[,3], angle = 90, code = 3, length = 0.02)
mtext(text = ylab, side = 2, line = 2)
mtext(text = 'Release Depth (m)', side = 1, line = 2.5)
legend(legpos, legend = legend_text, bty = 'n', fill = col_bars, cex = legencex)

# Barplot by bathy
bathy1 <- recruitment_bathy(dat1)
bathy2 <- recruitment_bathy(dat2)
bathy3 <- recruitment_bathy(dat3)
bathy <- cbind(bathy1[,1], bathy2[,1], bathy3[,1])
# bathy <- cbind(bathy1[,1], bathy2[,1])

bathyplot <- barplot(t(bathy), beside = T, ylim = ylim, col = col_bars)
arrows(bathyplot[1,], bathy1[,2], bathyplot[1,], bathy1[,3], angle = 90, code = 3, length = 0.02)
arrows(bathyplot[2,], bathy2[,2], bathyplot[2,], bathy2[,3], angle = 90, code = 3, length = 0.02)
arrows(bathyplot[3,], bathy3[,2], bathyplot[3,], bathy3[,3], angle = 90, code = 3, length = 0.02)
mtext(text = ylab, side = 2, line = 2)
mtext(text = 'Release Bathymetry (m)', side = 1, line = 2.5)
legend(legpos, legend = legend_text, bty = 'n', fill = col_bars, cex = legencex)

# Barplot by year
year1 <- recruitment_year(dat1)
year2 <- recruitment_year(dat2)
year3 <- recruitment_year(dat3)
year <- cbind(year1[,1], year2[,1], year3[,1])
# year <- cbind(year1[,1], year2[,1])

yearplot <- barplot(t(year), beside = T, ylim = ylim, col = col_bars, names.arg = years)
arrows(yearplot[1,], year1[,2], yearplot[1,], year1[,3], angle = 90, code = 3, length = 0.02)
arrows(yearplot[2,], year2[,2], yearplot[2,], year2[,3], angle = 90, code = 3, length = 0.02)
arrows(yearplot[3,], year3[,2], yearplot[3,], year3[,3], angle = 90, code = 3, length = 0.02)
mtext(text = ylab, side = 2, line = 2)
mtext(text = 'Release Year', side = 1, line = 2.5)
legend(legpos, legend = legend_text, bty = 'n', fill = col_bars, cex = legencex)

# Barplot by zone name
zone1 <- recruitment_zone(dat1)
zone2 <- recruitment_zone(dat2)
zone3 <- recruitment_zone(dat3)
zone <- cbind(zone1[,1], zone2[,1], zone3[,1])
# zone <- cbind(zone1[,1], zone2[,1])

zoneplot <- barplot(t(zone), beside = T, ylim = ylim, col = col_bars, names.arg = zonename)
arrows(zoneplot[1,], zone1[,2], zoneplot[1,], zone1[,3], angle = 90, code = 3, length = 0.02)
arrows(zoneplot[2,], zone2[,2], zoneplot[2,], zone2[,3], angle = 90, code = 3, length = 0.02)
arrows(zoneplot[3,], zone3[,2], zoneplot[3,], zone3[,3], angle = 90, code = 3, length = 0.02)
mtext(text = ylab, side = 2, line = 2)
mtext(text = 'Release Zone', side = 1, line = 2.5)
legend(legpos, legend = legend_text, bty = 'n', fill = col_bars, cex = legencex)

dev.off()