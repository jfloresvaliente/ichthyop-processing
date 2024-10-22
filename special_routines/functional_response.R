x <- seq(from = 0, to = 10, by = 0.1)
kx <- 1.6

f <- x/(x+kx)

png('C:/Users/jflores/Desktop/f.png', width = 850, height = 850, res = 120)
par(mar = c(5,5,1,1))
plot(x, f, type = 'l', xlab = '', lwd = 4, ylab = '', ylim = c(0,1), xaxs = 'i', yaxs = 'i', axes = F)
axis(side = 1, font = 2, lwd.ticks = 2, cex.axis = 1.5, lwd = 2)
axis(side = 2, font = 2, lwd.ticks = 2, cex.axis = 1.5, lwd = 2, las = 2)
mtext(side = 1, line = 3.2, font = 2, cex = 1.5, text = 'Mesozooplankton (umol CL-1)')
mtext(side = 2, line = 3.2, font = 2, cex = 1.5, text = 'Functional response (f)')
box(lwd = 2)
dev.off()

