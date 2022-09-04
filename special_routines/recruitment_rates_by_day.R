dirpath       <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu1/results/'
computeattime <- 91

day_released  <- NULL
day_recruited <- NULL

for(i in 1:computeattime){
  csv_file <- paste0(dirpath, 'ichthyop_output',i,'days.csv')
  dat <- read.table(csv_file, header = T, sep = ';')
  
  released  <- tapply(dat$NumberReleased,  list(dat$Month), sum)
  recruited <- tapply(dat$NumberRecruited, list(dat$Month), sum)
  
  day_released  <- rbind(day_released,  released)
  day_recruited <- rbind(day_recruited, recruited)
}

percent <- (day_recruited*100)/day_released


cols <- rep(c('red', 'orange', 'blue', 'green'), each = 3)
line_type <- rep(1:3, times = 4)
meses <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

png(paste0(dirpath, 'recruitment_rates_by_day.png'), width = 850, height = 850, res = 120)
# png(paste0('C:/Users/jflores/Desktop/', 'recruitment_rates_by_day.png'), width = 850, height = 850, res = 120)
par(mar = c(5,4,3,1))
plot(1, type = 'n', ylim = c(0,100), xlim = c(0,computeattime-1), xlab = '', ylab = '', main = '', yaxs = 'i', xaxs = 'i', axes = F)
axis(side = 1, font = 2, at = seq(0,computeattime,10), labels = seq(0,computeattime,10))
axis(side = 2, font = 2, at = seq(0,100,10), labels = seq(0,100,10), las = 2)
abline(v = seq(0,computeattime,10), h = seq(0,100,10), col = 'grey', lty = 2)
# abline(v = 30, lwd = 2, lty = 2)
# grid(col = 'grey', lty = 2, nx = 9, ny = 10)
mtext(side = 1, line = 2.5, font = 1.2, cex = 2, text = 'Days after spawning [d]')
mtext(side = 2, line = 2.5, font = 1.2, cex = 2, text = 'Recruitment [%]')
# mtext(side = 3, line = 0.5, font = 1.2, cex = 2, text = 'b)', adj = 0)
legend('topright', legend = meses, lty = line_type, bty = '', col = cols, ncol = 4, text.font = 2)
for(i in 1:12){
  lines(0:(computeattime-1), percent[,i], col = cols[i], lwd = 2, lty = line_type[i])
  # lines(0:39, percent[,i], col = cols[i], lwd = 2, lty = line_type[i])
}
box()
dev.off()
