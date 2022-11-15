#=============================================================================#
# Name   : plot_functional_response_curve
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : calculate and plot the temperature correction curve of the DEB model
# URL    : 
#=============================================================================#
dirpath  <- 'C:/Users/jflores/Desktop/'
out_name <- 'f_curve'
k_x <- .2

# Temperature range
X_max <- 20
X <- seq(from = 0, to = X_max, by = 0.01)
f <- X/(X + k_x)

text_size <- 1
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
png(filename = paste0(dirpath, out_name, k_x, '.png'), width = 850, height = 550, res = 120)
par(mar = c(4.5, 5.0, 1.5, 1.0))
plot(X, type = 'n', axes = F, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i', xlim = c(0,X_max), ylim = c(0,1))
axis(1, font = 2, lwd.ticks = 2, cex = 2, cex.axis = 1.5)
axis(2, font = 2, lwd.ticks = 2, cex = 2, cex.axis = 1.5, las = 2)
box(lwd = 2)
mtext(side = 1, line = 2.5, font = 2, cex = 1.5, text = 'Meso-zooplankton (µmol C/l)')
mtext(side = 2, line = 3.5, font = 2, cex = 1.5, text = 'Functional response (f)')

#===== CURVA 1 =====#
lines(X, f, lwd = 3)
# abline(v = k_x)

f2 <- abs(f - 0.75)
b <- which.min(f2)
f[b]
X[b]
abline(v = X[b])
abline(h = f[b])

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#
print(paste0('X = ', X[b]))
