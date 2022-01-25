dirpath       <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEBf1/k_x0/out/results/'
computeattime <- 91
N0            <- 1 # Initial value of the particle affected by mortality

recruitment_zone <- as.matrix(read.table('C:/Users/jflores/Documents/ICHTHYOP/ichthyop_recruitment_polygon.txt'))
source('ichthyop_libraries.R')
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
month_percent <- NULL 
for(j in 1:12){
  Rdata <- paste0(dirpath,'trajectoriesM',j,'.Rdata')
  load(Rdata)
  print(Rdata)
  dat <- trajectories; rm(trajectories)
  
  day_percent <- numeric(length = computeattime)
  
  for(i in 1:computeattime){
    
    if(i == 1){
      day_percent[i] <- 100
    }else{
      day <- as.matrix(subset(dat, dat$Timer == i)[,c(3,4)])
      particles <- dim(day)[1]
      
      day <- sum(in.out(bnd = recruitment_zone, x = day))
      day <- (day*100)/particles
      
      day_percent[i] <- day
    }
  }
  month_percent <- cbind(month_percent, day_percent)
}

cols <- rep(c('red', 'orange', 'blue', 'green'), each = 3)
line_type <- rep(1:3, times = 4)
meses <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

png(paste0(dirpath, 'retention_rates_by_day.png'), width = 850, height = 850, res = 120)
# png(paste0('C:/Users/jflores/Desktop/', 'retention_rates_by_day.png'), width = 850, height = 850, res = 120)
par(mar = c(5,4,3,1))
plot(1, type = 'n', ylim = c(0,100), xlim = c(0,computeattime-1), xlab = '', ylab = '', main = '', yaxs = 'i', xaxs = 'i', axes = F)
axis(side = 1, font = 2, at = seq(0,computeattime,10), labels = seq(0,computeattime,10))
axis(side = 2, font = 2, at = seq(0,100,10), labels = seq(0,100,10), las = 2)
abline(v = seq(0,computeattime,10), h = seq(0,100,10), col = 'grey', lty = 2)
abline(v = 30, lwd = 2, lty = 2)
# grid(col = 'grey', lty = 2, nx = 9, ny = 10)
mtext(side = 1, line = 2.5, font = 1.2, cex = 2, text = 'Days after spawning [d]')
mtext(side = 2, line = 2.5, font = 1.2, cex = 2, text = 'Recruitment [%]')
# mtext(side = 3, line = 0.5, font = 1.2, cex = 2, text = 'a)', adj = 0)
legend('topright', legend = meses, lty = line_type, bty = '', col = cols, ncol = 4, text.font = 2)
for(i in 1:12){
  lines(0:(computeattime-1), month_percent[,i], col = cols[i], lwd = 2, lty = line_type[i])
}
box()
dev.off()
