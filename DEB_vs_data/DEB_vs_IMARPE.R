deb_simu <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEB_4years_vari_Temp.txt'
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

#Internal function to calculate error bars
error_bar <- function(x, a = 0.05){
  # x = vector or matrix with dat1a to evaluate
  
  if(!is.null(a)){
    n  <- length(x)
    m  <- mean(x, na.rm = T)
    tt <- -qt(p = a/2, df = n-1)
    ee <- sd(x)/sqrt(n)  # standard error
    e  <- tt*ee          # error range
    d  <- e/m            # relative error, says that the confidence interval is a percentage of the value
    li <- m-e            # lower limit
    ls <- m+e            # upper limit
    stat <- c(m, li, ls)
  }else{
    n  <- length(x)
    m  <- mean(x, na.rm = T)
    tt <- -qt(p = a/2, df = n-1)
    ee <- sd(x)/sqrt(n)  # standard error
    li <- m-ee           # lower limit
    ls <- m+ee           # upper limit
    stat <- c(m, li, ls)
  }
  return(stat)
}

tim <- dat1$t
m <- dat1[,c(2:12)]
m <- apply(X = m, MARGIN = 1, FUN = error_bar)

png('C:/Users/jflores/Desktop/DEBmean_vs_IMARPE.png', width = 850, height = 850, res = 120)
par(mar = c(4,4,1,1))
plot(tim, m[1,], type = 'n', xlab = '', ylab = '', xlim = c(0,35), ylim = c(0,2.5), axes = F)
axis(1, lwd = 2, font = 2)
axis(2, lwd = 2, font = 2, las = 2)
box(lwd = 2)
mtext(side = 1, line = 2.5, font = 2, text = 'Age in days [d]')
mtext(side = 2, line = 2.5, font = 2, text = 'Standard length [cm]')

polygon(x = c(tim, rev(tim)), y = c(m[2,], rev(m[3,])), border = NA, col = 'grey80')
lines(tim, m[1,], lwd = 2)
lines(tim, dat1[,2] , lty = 2, col = 'grey80')
lines(tim, dat1[,12], lty = 2, col = 'grey80')

for(i in 1:length(temps)){

  # Points
  sub2 <- subset(dat2, dat2$temp == temps[i])
  points(sub2$t_days, sub2$sl_cm, col = cols[i], cex = .75)
}

legend('topleft', bty = 'n',
       xjust = 0, yjust = 0,
       y.intersp = .9, x.intersp = .5,
       legend = c('IMARPE 14ºC', 'IMARPE 18ºC'), text.font = 2,
       text.col = cols, pch = 1, col = cols)
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#