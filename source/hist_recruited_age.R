dirpath <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/MESO60dias/results_DEB/'

days <- 60
png(paste0(dirpath, 'hist_recruited_age.png'), width = 1450, height = 850, res = 120)

densidades <- list()
par(mfrow = c(3,4), mar = c(5,4,1,1))
for(i in 1:12){
  load(paste0(dirpath, 'trajectoriesM', i, '.Rdata'))
  dat <- trajectories; rm(trajectories)
  
  index <- subset(dat, dat$Timer == (days+1) & dat$IfRecruited == 1)$Drifter
  recruited <- subset(dat, dat$Drifter %in% index)
  recruited <- matrix(data = recruited$IfRecruited, ncol = (days+1), nrow = dim(recruited)[1]/(days+1), byrow = T)
  recruited_day <- apply(recruited, 1, which.max)
  hist(recruited_day, breaks = seq(0,70,1),
       ylim = c(0, 2000), xlim = c(0,70),
       xlab = '', ylab = '', main = '')
  mtext(text = paste('Month', i), side = 3, line = -2, adj = 0.1)
  mtext(text = 'Recruited Age', side = 1, line = 2.5)
  
  densidades[[i]] <- density(recruited_day)
}
dev.off()

png(paste0(dirpath, 'density_recruited_age.png'), width = 1300, height = 850, res = 120)
cols <- rep(c('red', 'orange', 'blue', 'green'), each = 3)
lins <- rep(1:3, times = 4)
plot(1, type = 'n', ylim = c(0, 0.1), xlim = c(0,70), xlab = '', ylab = '', main = '')
mtext(text = 'Recruited Age', side = 1, line = 2.5)
for(i in 1:12){
  dens <- densidades[[i]]
  lines(dens, col = cols[i], lwd = 2, lty = lins[i])
}
legend('topleft', legend = 1:12, lty = lins, bty = 'n', col = cols, ncol = 2)
dev.off()
