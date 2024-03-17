#=============================================================================#
# Name   : plot_bars_age_lines_size_mortality
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Barplot (age criterion / retention), lines (size criterion & mortality)
# URL    : 
#=============================================================================#
source('ichthyop_functions.R')
dirpath   <- 'E:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj/out_case1/results/' # Dirpath
retention <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/FISICA/out/results_30days/ichthyop_output.csv' # .csv retention file
lats      <- seq(from = 2, to = 20, by = 2)
ylab      <- 'Recruitment (%)'
ymax1     <- c(0,65)
ymax2     <- c(0,0.1)
yticks1 <- seq(ymax1[1],ymax1[2],10)
yticks2 <- seq(ymax2[1],ymax2[2],.05)
legend_text <- c('Age criterion','Size criterion','Size criterion + Constant mortality')

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

# Generate latitude names
latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º - ', lats[i] + 2, 'º'))

# Larval retention data
dat1 <- read.table(retention, header = T, sep = ';')

# Larval recruitment data
dat2 <- read.table(paste0(dirpath, '/ichthyop_output2.csv'), header = T, sep = ';')

# Larval recruitment data + constant mortality data
dat3 <- read.table(paste0(dirpath, '/ichthyop_output2.csv'), header = T, sep = ';')
dat3$Recruitprop <- dat3$N_constantprop # choose the type of mortality

# Get mean values and confidence intervals for each factor
month1 <- recruitment_month(dat1)
month2 <- recruitment_month(dat2)
month3 <- recruitment_month(dat3)

depth1 <- recruitment_depth(dat1)
depth2 <- recruitment_depth(dat2)
depth3 <- recruitment_depth(dat3)

bathy1 <- recruitment_bathy(dat1)
bathy2 <- recruitment_bathy(dat2)
bathy3 <- recruitment_bathy(dat3)

zone1  <- recruitment_zone(dat1)
zone2  <- recruitment_zone(dat2)
zone3  <- recruitment_zone(dat3)

png(paste0(dirpath, 'plot_bars_age_lines_size_mortality.png'), height = 850, width = 1450, res = 120)
par(mfrow = c(2,2))

mycol1 <- adjustcolor(col = 'grey', alpha.f = 0.9) # grey color for bars in first axis
mycol2 <- adjustcolor(col = 'grey', alpha.f = 0.0) # transparent color for second axis

#========================= Plot by spawning month =========================#
par(mar = c(3.5,4,.5,4))

plot1 <- barplot(month1[,1], ylim = ymax1, axes = F, names.arg = F, border = mycol2, col = mycol1)
mtext(side = 1, line = 2  , cex = 1.3, font = 2, text = 'Spawning Month')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5,    font = 2, text = 'a)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = plot1, labels = 1:12)
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)
arrows(plot1, month1[,2], plot1, month1[,3], angle = 90, code = 3, length = 0.02)

# dat2 curve
lines(plot1, month2[,1], lwd = 3)
points(plot1, month2[,1], pch = 17, cex = 1.2)
arrows(plot1, month2[,2], plot1, month2[,3], angle = 90, code = 3, length = 0.02)

par(new = T)
plot1 <- barplot(month3[,1], axes = F, ylim = ymax2, col = mycol2, border = mycol2, names.arg = F)

# dat3 curve
lines(plot1-.13, month3[,1], lwd = 3, col = 'red')
points(plot1-.13, month3[,1], pch = 16, cex = 1.2, col = 'red')
arrows(plot1-.13, month3[,2], plot1-.13, month3[,3], angle = 90, code = 3, length = 0.02, col = 'red')

axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)

# legend('topright',
#        lty = c(0,1,1,2),
#        lwd = c(0,2,2,2),
#        col = c(0,1,'red','red'),
#        bty = 'n',
#        pch = c(0,17,16,16),
#        fill = c(mycol1,mycol2,mycol2,mycol2),
#        border = c(mycol1,mycol2,mycol2,mycol2),
#        seg.len = 5,
#        pt.cex  = 1.5,
#        legend  = legend_text,
#        text.font = 2,
#        cex = 0.9
#        )

legend('topright',
       lty = c(0,1,1),
       lwd = c(0,2,2),
       col = c(0,1,'red'),
       bty = 'n',
       pch = c(0,17,16),
       fill = c(mycol1,mycol2,mycol2),
       border = c(mycol1,mycol2,mycol2),
       seg.len = 5,
       pt.cex  = 1.5,
       legend  = legend_text,
       text.font = 2,
       cex = 0.9
)

#========================= Plot by spawning latitude =========================#
par(mar = c(3.5,4,.5,4))

plot1 <- barplot(zone1[,1], ylim = ymax1, axes = F, names.arg = F, border = mycol2, col = mycol1)
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Latitude')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'b)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = plot1, labels = rep('',9))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)
text(plot1, -ymax1[2]/20, labels = latlab, srt = 20, xpd = TRUE, cex = 1, font = 2)
arrows(plot1, zone1[,2], plot1, zone1[,3], angle = 90, code = 3, length = 0.02)

# dat2 curve
lines(plot1, zone2[,1], lwd = 3)
points(plot1, zone2[,1], pch = 17, cex = 1.2)
arrows(plot1, zone2[,2], plot1, zone2[,3], angle = 90, code = 3, length = 0.02)

par(new = T)
plot1 <- barplot(zone2[,1], axes = F, ylim = ymax2, col = mycol2, border = mycol2, names.arg = F)

# dat3 curve
lines(plot1-.15, zone3[,1], lwd = 3, col = 'red')
points(plot1-.15, zone3[,1], pch = 16, cex = 1.2, col = 'red')
arrows(plot1-.15, zone3[,2], plot1-.15, zone3[,3], angle = 90, code = 3, length = 0.02, col = 'red')

axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)

#========================= Plot by spawning depth =========================#
par(mar = c(3.5,4,.5,4))

plot1 <- barplot(depth1[,1], ylim = ymax1, axes = F, names.arg = F, border = mycol2, col = mycol1)
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Depth [m]')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'c)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = plot1, labels = rownames(depth1))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)
arrows(plot1, depth1[,2], plot1, depth1[,3], angle = 90, code = 3, length = 0.02)

# dat2 curve
lines(plot1, depth2[,1], lwd = 3)
points(plot1, depth2[,1], pch = 17, cex = 1.2)
arrows(plot1, depth2[,2], plot1, depth2[,3], angle = 90, code = 3, length = 0.02)

par(new = T)
plot1 <- barplot(depth3[,1], axes = F, ylim = ymax2, col = mycol2, border = mycol2, names.arg = F)

# dat3 curve
lines(plot1-.09, depth3[,1], lwd = 3, col = 'red')
points(plot1-.09, depth3[,1], pch = 16, cex = 1.2, col = 'red')
arrows(plot1-.09, depth3[,2], plot1-.09, depth3[,3], angle = 90, code = 3, length = 0.02, col = 'red')

axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)

#========================= Plot by spawning bathymetry =========================#
par(mar = c(3.5,4,.5,4))

plot1 <- barplot(bathy1[,1], ylim = ymax1, axes = F, names.arg = F, border = mycol2, col = mycol1)
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Bathymetry [m]')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'd)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = plot1, labels = rownames(bathy1))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)
arrows(plot1, bathy1[,2], plot1, bathy1[,3], angle = 90, code = 3, length = 0.02)

# dat2 curve
lines(plot1, bathy2[,1], lwd = 3)
points(plot1, bathy2[,1], pch = 17, cex = 1.2)
arrows(plot1, bathy2[,2], plot1, bathy2[,3], angle = 90, code = 3, length = 0.02)

par(new = T)
plot1 <- barplot(bathy3[,1], axes = F, ylim = ymax2, col = mycol2, border = mycol2, names.arg = F)

# dat3 curve
lines(plot1-.09, bathy3[,1], lwd = 3, col = 'red')
points(plot1-.09, bathy3[,1], pch = 16, cex = 1.2, col = 'red')
arrows(plot1-.09, bathy3[,2], plot1-.09, bathy3[,3], angle = 90, code = 3, length = 0.02, col = 'red')

axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#