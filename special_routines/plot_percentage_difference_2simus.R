#=============================================================================#
# Name   : plot_percentage_difference_2simus
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Barplot (age criterion / retention), lines (size criterion & mortality)
# URL    : 
#=============================================================================#
source('ichthyop_functions.R')

dirpath1  <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/case1/results/' # f variable
dirpath2  <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5_TC0.053/case1/results/' # f = 1

lats      <- seq(from = 2, to = 20, by = 2)
ylab      <- 'Percentage of variation (%)'
ymax1     <- c(0,5)
ymax2     <- c(0,20)
yticks1 <- seq(ymax1[1],ymax1[2],1)
yticks2 <- seq(ymax2[1],ymax2[2],5)
legend_text <- c('Size criterion','Size criterion + Constant mortality')

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

# Generate latitude names
latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º - ', lats[i] + 2, 'º'))

# f variable
dat1 <- read.table(paste0(dirpath1, '/ichthyop_output2.csv'), header = T, sep = ';')
dat2 <- dat1; dat2$Recruitprop <- dat2$N_constantprop # choose the type of mortality

# f = 1
dat3 <- read.table(paste0(dirpath2, '/ichthyop_output2.csv'), header = T, sep = ';')
dat4 <- dat3; dat4$Recruitprop <- dat4$N_constantprop # choose the type of mortality

month1 <- recruitment_month(dat1)[,1]
month2 <- recruitment_month(dat2)[,1]
month3 <- recruitment_month(dat3)[,1]
month4 <- recruitment_month(dat4)[,1]

depth1 <- recruitment_depth(dat1)[,1]
depth2 <- recruitment_depth(dat2)[,1]
depth3 <- recruitment_depth(dat3)[,1]
depth4 <- recruitment_depth(dat4)[,1]

bathy1 <- recruitment_bathy(dat1)[,1]
bathy2 <- recruitment_bathy(dat2)[,1]
bathy3 <- recruitment_bathy(dat3)[,1]
bathy4 <- recruitment_bathy(dat4)[,1]

zone1  <- recruitment_zone(dat1)[,1]
zone2  <- recruitment_zone(dat2)[,1]
zone3  <- recruitment_zone(dat3)[,1]
zone4  <- recruitment_zone(dat4)[,1]

mycol <- adjustcolor(col = 'grey', alpha.f = 0.0) # transparent color for second axis

png(paste0('C:/Users/jflores/Desktop/','plot_bars_age_lines_size_mortalityXXX.png'), height = 850, width = 1450, res = 120)
par(mfrow = c(2,2))

#========================= Plot by spawning month =========================#
par(mar = c(3.5,4,.5,4))

plot1 <- barplot(month1, ylim = ymax1, axes = F, names.arg = F, border = mycol, col = mycol)
mtext(side = 1, line = 2  , cex = 1.3, font = 2, text = 'Spawning Month')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5,    font = 2, text = 'a)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = plot1, labels = names(month1))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)

vari_month <- ((month3 - month1)/month1) * 100
# vari_month <- month3 - month1
lines(plot1, vari_month, lwd = 3, lty = 1, col = 'black')

par(new = T)
plot1 <- barplot(month1, ylim = ymax2, axes = F, names.arg = F, border = mycol, col = mycol)
vari_month <- ((month4 - month2)/month2) * 100
# vari_month <- month4 - month2

lines(plot1, vari_month, lwd = 3, lty = 2, col = 'red')
axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)

legend('bottom',
       lty = c(1,2),
       lwd = c(3,3),
       col = c('black','red'),
       bty = 'n',
       # pch = c(0,17,16,16),
       # fill = c(mycol,mycol,mycol,mycol),
       # border = c(mycol,mycol,mycol,mycol),
       seg.len = 5,
       pt.cex  = 1.5,
       legend  = legend_text,
       text.font = 2,
       cex = 0.9
       )

# legend('topright',
#        lty = c(0,1,1),
#        lwd = c(0,2,2),
#        col = c(0,1,'red'),
#        bty = 'n',
#        pch = c(0,17,16),
#        fill = c(mycol,mycol,mycol),
#        border = c(mycol,mycol,mycol),
#        seg.len = 5,
#        pt.cex  = 1.5,
#        legend  = legend_text,
#        text.font = 2,
#        cex = 0.9
# )

#========================= Plot by spawning latitude =========================#
par(mar = c(3.5,4,.5,4))

plot1 <- barplot(zone1, ylim = ymax1, axes = F, names.arg = F, border = mycol, col = mycol)
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Latitude')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = '')
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'b)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = plot1, labels = rep('',9))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)
text(plot1, -ymax1[2]/20, labels = latlab, srt = 20, xpd = TRUE, cex = 1, font = 2)

vari_zone <- ((zone3 - zone1)/zone1) * 100
lines(plot1, vari_zone, lwd = 3, lty = 1, col = 'black')

par(new = T)
plot1 <- barplot(zone1, ylim = ymax2, axes = F, names.arg = F, border = mycol, col = mycol)
vari_zone <- ((zone4 - zone2)/zone2) * 100
lines(plot1, vari_zone, lwd = 3, lty = 2, col = 'red')
axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)

#========================= Plot by spawning depth =========================#
par(mar = c(3.5,4,.5,4))

plot1 <- barplot(depth1, ylim = ymax1, axes = F, names.arg = F, border = mycol, col = mycol)
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Depth [m]')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'c)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = plot1, labels = names(depth1))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)

vari_depth <- ((depth3 - depth1)/depth1) * 100
lines(plot1, vari_depth, lwd = 3, lty = 1, col = 'black')

par(new = T)
plot1 <- barplot(depth1, ylim = ymax2, axes = F, names.arg = F, border = mycol, col = mycol)
vari_depth <- ((depth4 - depth2)/depth2) * 100
lines(plot1, vari_depth, lwd = 3, lty = 2, col = 'red')
axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)

#========================= Plot by spawning bathymetry =========================#
par(mar = c(3.5,4,.5,4))

plot1 <- barplot(bathy1, ylim = ymax1, axes = F, names.arg = F, border = mycol, col = mycol)
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Bathymetry [m]')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = '')
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'd)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = plot1, labels = names(bathy1))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks1, labels = yticks1, las = 2)

vari_bathy <- ((bathy3 - bathy1)/bathy1) * 100
lines(plot1, vari_bathy, lwd = 3, lty = 1, col = 'black')

par(new = T)
plot1 <- barplot(bathy1, ylim = ymax2, axes = F, names.arg = F, border = mycol, col = mycol)
vari_bathy <- ((bathy4 - bathy2)/bathy2) * 100
lines(plot1, vari_bathy, lwd = 3, lty = 2, col = 'red')
axis(side = 4, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks2, labels = yticks2, las = 2, col.axis = 'red', col = 'red', line = 0)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#