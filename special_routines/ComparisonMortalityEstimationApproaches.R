#=============================================================================#
# Name   : ComparisonMortalityEstimationApproaches
# Author : Jorge Flores-Valiente
# Date   :
# Version:
# Aim    :
# URL    :
#=============================================================================#

# Enfoque Jorge Flores
init  <- 1
z     <- 0.1
age   <- 90
tim   <- 0:(age-1) # edad en dias

mort1 <- NULL
for(i in 1:(age-1)){
  val  <- init * (1 - z)
  init <- val
  mort1 <- c(mort1, val)
}

# Enfoque Garavelli et al 2016
mort2 <- (1 - z)^(1:(age-1))

# Agregar valor inicial de cada particula = 1
mort1 <- c(1, mort1)
mort2 <- c(1, mort2)

# Grafico
png(filename = 'C:/Users/jflores/Desktop/mortality_curve.png', width = 850, height = 850, res = 120)
par(mar = c(4.5,5,1.5,1))
plot(tim, mort1, type = 'l', xlim = c(0,age), ylim = c(0,1.1), xlab = '', ylab = '', axes = F, lwd = 2)
lines(tim, mort2, col = 'red')
axis(1, font = 2, lwd.ticks = 2, cex = 2, cex.axis = 1.5)
axis(2, font = 2, lwd.ticks = 2, cex = 2, cex.axis = 1.5, las = 2)
box(lwd = 2)
mtext(side = 1, line = 2.5, font = 2, cex = 1.5, text = 'Age (d)')
mtext(side = 2, line = 3.5, font = 2, cex = 1.5, text = 'Individual Worth')
dev.off()
