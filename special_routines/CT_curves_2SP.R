#=============================================================================#
# Name   : CT_curves_2SP
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Calculate CT for DEB model (E. encrasicolus, E. ringens)
# URL    : 
#=============================================================================#
# First run: CT_curves_Engraulis_encrasicolus.R & CT_curves_Engraulis_ringens.R

png(filename = 'C:/Users/jflores/Desktop/CTcurves_2SP.png', width = 850, height = 550, res = 120)
par(mar = c(4.5,5,1.5,1))
plot(Temp, type = 'n', axes = F, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i', xlim = c(0,35), ylim = c(0,3))
axis(1, font = 2, lwd.ticks = 2, cex = 2, cex.axis = 1.5)
axis(2, font = 2, lwd.ticks = 2, cex = 2, cex.axis = 1.5, las = 2)
box(lwd = 2)
mtext(side = 1, line = 2.5, font = 2, cex = 1.5, text = 'Temperature (ºC)')
mtext(side = 2, line = 3.5, font = 2, cex = 1.5, text = 'Correction factor')

# encrasicolus lines
lines(encrasicolus$Temp, encrasicolus$Case2, lwd = 4.5, col = 'red')
lines(encrasicolus$Temp, encrasicolus$Case1, lwd = 3.0, col = 'blue')
legend('topleft', col = c('blue','red'), lty = 1, lwd = 2, legend = c('Case 1', 'Case 2'), bty = 'n', title = expression(bold('DEB'[std])))

# ringens lines
lines(ringens$Temp, ringens$Case2, lwd = 4.5, col = 'red', lty = 2)
lines(ringens$Temp, ringens$Case1, lwd = 3.0, col = 'blue', lty = 2)
legend('topright', col = c('blue','red'), lty = 2, lwd = 2, legend = c('Case 1', 'Case 2'), bty = 'n', title = expression(bold('DEB'[abj])))

dev.off()
