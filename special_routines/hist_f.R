dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB/k_x1.6/out/results/'
days <- 90 # Paso de tiempo

# No cambiar nada desde aqui
f_mean_list <- list()
f_sd_list   <- list()

for(i in 1:12){
  
  Rfile <- paste0(dirpath, 'trajectoriesM', i, '.Rdata')
  print(Rfile)
  load(Rfile)
  
  dat <- trajectories; rm(trajectories)
  dat$f <- dat$MESO/(dat$MESO + 1.6)
  
  index <- subset(dat, dat$Timer == (days+1) & dat$IfRecruited == 1)$Drifter
  dat <- subset(dat, dat$Drifter %in% index)
  dat <- matrix(data = dat$f, ncol = (days+1), byrow = T)
  
  f_mean <- apply(X = dat, MARGIN = c(2), FUN = mean)
  f_sd   <- apply(X = dat, MARGIN = c(2), FUN = sd)
  
  # Para guardar la informacion util para plotear
  f_mean_list[[i]] <- f_mean
  f_sd_list[[i]]   <- f_sd
}

cols <- rep(c('red', 'orange', 'blue', 'green'), each = 3)
line_type <- rep(1:3, times = 4)
meses <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Agu','Sep','Oct','Nov','Dec')

ylim = c(.2,.6)
png(paste0(dirpath, 'f_mean_age.png'), width = 950, height = 850, res = 120)
par(mar = c(5,4,1,1))
# plot(1, xlim = c(0,days), ylim = c(0.43,.63), type = 'n', xlab = '', ylab = '')
plot(1, xlim = c(0,days), ylim = ylim, type = 'n', xlab = '', ylab = '', axes = F)
axis(1)
axis(2, las = 2)
box()
abline(h = 0.5, lwd = 2, col = 'grey', lty = 2)
mtext(side = 1, line = 2.5, text = 'Age [days]')
mtext(side = 2, line = 2.5, text = 'Functional response (f)')
for(i in 1:12){
  lines(x = 0:(days), f_mean_list[[i]], col = cols[i], lwd = 2, lty = line_type[i])
}
legend('topright', legend = meses, lty = line_type, bty = 'n', col = cols, ncol = 2)
dev.off()
