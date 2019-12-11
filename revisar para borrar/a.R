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

dat1 <- read.table('D:/ICHTHYOP/peru02km/Bathy/out/results/ichthyop_output.csv', header = T, sep = ';')
dat2 <- read.table('E:/ICHTHYOP/peru10km/Bathy/out/results/ichthyop_output.csv', header = T, sep = ';')

legend_text = c('02 km', '10 km')
ylim = c(0,60)
ylab = 'Local Retention (%)'
col_bars <- c('grey20','grey80')
legpos <- 'topright'
years <- c('Y1', 'Y2', 'Y3')
zonename <- c('6º-8º','8º-10º','10º-12º','12º-14º')
legencex <- 1

png('C:/Users/jflores/Desktop/ichthyop_results.png', width = 1250, height = 850, res = 120)
par(mfrow = c(2,3))

# Barplot by day
day1 <- recruitment_day(dat1)
day2 <- recruitment_day(dat2)
day <- cbind(day1[,1], day2[,1])

dayplot <- barplot(t(day), beside = T, ylim = ylim, col = col_bars)
arrows(dayplot[1,], day1[,2], dayplot[1,], day1[,3], angle = 90, code = 3, length = 0.02)
arrows(dayplot[2,], day2[,2], dayplot[2,], day2[,3], angle = 90, code = 3, length = 0.02)
mtext(text = ylab, side = 2, line = 2)
mtext(text = 'Release Month', side = 1, line = 2.5)
legend(legpos, legend = legend_text, bty = 'n', fill = col_bars, cex = legencex)

# Barplot by depth
depth1 <- recruitment_depth(dat1)
depth2 <- recruitment_depth(dat2)
depth <- cbind(depth1[,1], depth2[,1])

depthplot <- barplot(t(depth), beside = T, ylim = ylim, col = col_bars, names.arg = rownames(depth1))
arrows(depthplot[1,], depth1[,2], depthplot[1,], depth1[,3], angle = 90, code = 3, length = 0.02)
arrows(depthplot[2,], depth2[,2], depthplot[2,], depth2[,3], angle = 90, code = 3, length = 0.02)
mtext(text = ylab, side = 2, line = 2)
mtext(text = 'Release Depth (m)', side = 1, line = 2.5)
legend(legpos, legend = legend_text, bty = 'n', fill = col_bars, cex = legencex)

# Barplot by bathy
bathy1 <- recruitment_bathy(dat1)
bathy2 <- recruitment_bathy(dat2)
bathy <- cbind(bathy1[,1], bathy2[,1])

bathyplot <- barplot(t(bathy), beside = T, ylim = ylim, col = col_bars)
arrows(bathyplot[1,], bathy1[,2], bathyplot[1,], bathy1[,3], angle = 90, code = 3, length = 0.02)
arrows(bathyplot[2,], bathy2[,2], bathyplot[2,], bathy2[,3], angle = 90, code = 3, length = 0.02)
mtext(text = ylab, side = 2, line = 2)
mtext(text = 'Release Bathymetry (m)', side = 1, line = 2.5)
legend(legpos, legend = legend_text, bty = 'n', fill = col_bars, cex = legencex)

# Barplot by year
year1 <- recruitment_year(dat1)
year2 <- recruitment_year(dat2)
year <- cbind(year1[,1], year2[,1])

yearplot <- barplot(t(year), beside = T, ylim = ylim, col = col_bars, names.arg = years)
arrows(yearplot[1,], year1[,2], yearplot[1,], year1[,3], angle = 90, code = 3, length = 0.02)
arrows(yearplot[2,], year2[,2], yearplot[2,], year2[,3], angle = 90, code = 3, length = 0.02)
mtext(text = ylab, side = 2, line = 2)
mtext(text = 'Release Year', side = 1, line = 2.5)
legend(legpos, legend = legend_text, bty = 'n', fill = col_bars, cex = legencex)

# Barplot by zone name
zone1 <- recruitment_zone(dat1)
zone2 <- recruitment_zone(dat2)
zone <- cbind(zone1[,1], zone2[,1])

zoneplot <- barplot(t(zone), beside = T, ylim = ylim, col = col_bars, names.arg = zonename)
arrows(zoneplot[1,], zone1[,2], zoneplot[1,], zone1[,3], angle = 90, code = 3, length = 0.02)
arrows(zoneplot[2,], zone2[,2], zoneplot[2,], zone2[,3], angle = 90, code = 3, length = 0.02)
mtext(text = ylab, side = 2, line = 2)
mtext(text = 'Release Zone', side = 1, line = 2.5)
legend(legpos, legend = legend_text, bty = 'n', fill = col_bars, cex = legencex)

dev.off()

###############################-------#
# mod <- lm(Recruitprop ~
#             factor(Year) + factor(Day) + factor(Zone_name) + factor(Depth) + factor(Bathy)
#           + factor(Year):factor(Day) + factor(Year):factor(Zone_name) + factor(Year):factor(Depth) + factor(Year):factor(Bathy)
#           + factor(Day):factor(Zone_name)+ factor(Day):factor(Depth)+ factor(Day):factor(Bathy)
#           + factor(Zone_name):factor(Depth) + factor(Zone_name):factor(Bathy)
#           + factor(Depth):factor(Bathy)
#           ,data = dat1)

mod <- lm(Recruitprop ~
            factor(Year) + factor(Day) + factor(Zone_name) + factor(Bathy)
          + factor(Year):factor(Day) + factor(Year):factor(Zone_name) + factor(Year):factor(Bathy)
          + factor(Day):factor(Zone_name)+ factor(Day):factor(Bathy)
          + factor(Zone_name):factor(Bathy)
          ,data = dat1)

# summary(mod)
aov = anova(mod)
print(aov)
print(100 * aov[2] / sum(aov[2]))
aov_sum <- (100 * aov[2] / sum(aov[2])); colnames(aov_sum) <- '%Exp'

aov <- cbind(aov,aov_sum)
# rownames(aov) <- c('year','day','depth','age',
                   # 'year x Day','year x depth','year x age',
                   # 'Day x depth','Day x age',
                   # 'depth x age','residuals')
write.csv(as.matrix(aov), file = paste0('C:/Users/jflores/Desktop/ANOVA.csv'), na = "")
