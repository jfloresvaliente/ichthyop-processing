deb_simu <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEB_4years_vari_Temp.txt'
fishbase <- 'C:/Users/jflores/Desktop/imarpe_Eringens.csv'

dat1 <- read.table(deb_simu, header = F, sep = ',')
dat1 <- dat1[seq(1,dim(dat1)[1],100), ]
dat1 <- subset(dat1, dat1[,1] < 35)
colnames(dat1) <- c('t', 10:20)

dat2 <- read.table(fishbase, header = T, sep = ';')
dat2$temp <- as.factor(dat2$temp)

png('C:/Users/jflores/Desktop/DEB_vs_IMARPE.png', width = 850, height = 850, res = 120)
par(mar = c(4,4,1,1))
plot(1, type = 'n', axes = F,
     xlab = '', ylab = '',
     xlim = c(0,35), ylim = c(0,2.5))
mtext(side = 1, line = 2.5, font = 2, text = 'Age in days [d]')
mtext(side = 2, line = 2.5, font = 2, text = 'Standard length [cm]')
axis(1, lwd = 2, font = 2)
axis(2, lwd = 2, font = 2, las = 2)
box(lwd = 2)

cols <- c('blue', 'red')
temps <- c(14,18)
for(i in 1:length(temps)){
  # Lines
  sub1 <- which(names(dat1) == temps[i])
  sub1 <- cbind(dat1$t, dat1[,sub1])
  lines(sub1[,1], sub1[,2], col = cols[i], lwd = 3)
  
  # Points
  sub2 <- subset(dat2, dat2$temp == temps[i])
  points(sub2$t_days, sub2$sl_cm, col = cols[i], cex = .75)
}

legend('topleft', bty = 'n',
       xjust = 0, yjust = 0,
       y.intersp = .9, x.intersp = .5,
       legend = c('DEB 14ºC', 'DEB 18ºC'), text.font = 2,
       text.col = cols, lty = 1, col = cols)

legend('topright', bty = 'n',
       xjust = 0, yjust = 0,
       y.intersp = .9, x.intersp = .5,
       legend = c('IMARPE 14ºC', 'IMARPE 18ºC'), text.font = 2,
       text.col = cols, pch = 1, col = cols)

dev.off()

#=============================================================================#
# END OF PROGRAM
#=============================================================================#