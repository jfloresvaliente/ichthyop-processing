dirpath <- 'E:/ICHTHYOP/10kmparent/DEB/out/E_ringens/results/'
days <- 60 # Paso de tiempo

# No cambiar nada desde aqui
reclutas_freq <- list()
reclutas_dens <- list()

reclutas_breaks  <- NULL
reclutas_counts  <- NULL
reclutas_density <- NULL

for(i in 1:12){
  
  Rfile <- paste0(dirpath, 'trajectoriesM', i, '.Rdata')
  print(Rfile)
  load(Rfile)
  
  dat <- trajectories; rm(trajectories)
  dat <- subset(dat, dat$Timer <= (days+1))
  
  index <- subset(dat, dat$Timer == (days+1) & dat$IfRecruited == 1)$Drifter
  recruited <- subset(dat, dat$Drifter %in% index)
  recruited <- matrix(data = recruited$IfRecruited, ncol = (days+1), nrow = dim(recruited)[1]/(days+1), byrow = T)
  recruited_day    <- apply(recruited, 1, which.max)

  # Para obtener el ylim de los plots  
  reclutas_hist    <- hist(recruited_day, seq(0,days+1,1), plot = F)
  reclutas_breaks  <- c(reclutas_breaks , reclutas_hist$breaks)
  reclutas_counts  <- c(reclutas_counts , reclutas_hist$counts)
  reclutas_density <- c(reclutas_density, density(recruited_day)$y)

  # Para guardar la informacion util para plotear
  reclutas_freq[[i]] <- recruited_day
  reclutas_dens[[i]] <- density(recruited_day)
}

# Plot Frecuencia absoluta de reclutados
xlim <- c(10, ceiling(max(reclutas_breaks)/5)  * 5)
ylim <- c(0 , ceiling(max(reclutas_counts)/50) * 50)

png(paste0(dirpath, 'hist_recruited_age.png'), width = 1450, height = 850, res = 120)
par(mfrow = c(3,4), mar = c(3.5,4,.5,.5))
for(i in 1:12){
  hist_plot <- reclutas_freq[[i]]
  hist(hist_plot, breaks = seq(0,days+1,1),
       ylim = ylim, xlim = xlim,
       xlab = '', ylab = '', main = '')
  mtext(text = paste('Month', i), side = 3, line = -2, adj = 0.1)
  mtext(side = 1, line = 2.5, cex = .75, text = 'Recruited Age [d]')
  mtext(side = 2, line = 2.5, cex = .75, text = 'Recruited Particles [#]')
}
dev.off()

# Density Plot
ylim <- c(0, round(max(reclutas_density), 2))

cols <- rep(c('red', 'orange', 'blue', 'green'), each = 3)
lins <- rep(1:3, times = 4)

png(paste0(dirpath, 'density_recruited_age.png'), width = 950, height = 850, res = 120)
par(mar = c(5,4,1,1))
plot(1, type = 'n', ylim = ylim, xlim = xlim, xlab = '', ylab = '', main = '')
mtext(side = 1, line = 2.5, text = 'Recruited Age')
mtext(side = 2, line = 2.5, text = 'Density')

for(i in 1:12){
  dens <- reclutas_dens[[i]]
  lines(dens, col = cols[i], lwd = 2, lty = lins[i])
}
legend('topleft', legend = 1:12, lty = lins, bty = 'n', col = cols, ncol = 2)
dev.off()
