dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/bib_data/'

lab <- read.table(file = paste0(dirpath, 'Santander et al 1984 tabla2.csv'), header = T, sep = ';')
lab$PesoGonada <- lab[,1] - lab[,2]
lab$GI <- (lab$PesoGonada/lab[,1])*100

# Modelo de regresion linear para relacionar el Peso Total con el Peso sin Gonada
# Peso Total = a + b*(Peso sin Gonada)
mod1 <- lm(formula = PesoTotal ~ PesoSinGonada, data = lab)
cof <- mod1$coefficients
mod1X <- round(range(lab[,2])[1] : range(lab[,2])[2])
mod1Y <- cof[1] + cof[2] * mod1X

png_name <- 'C:/Users/jflores/Desktop/Santander 1984 Tabla 2.png'
png(filename = png_name, width = 1000, height = 400, res = 120)

# Plot de Peso sin Gonada vs Peso Total
par(mfrow = c(1,4),mar = c(4, 4, 1, 1), lwd = 2)
plot(lab[,2], lab[,1], xlab = '', ylab = '', axes = F)
axis(1, font = 2, lwd.ticks = 2)
axis(2, font = 2, lwd.ticks = 2, las = 2)
mtext(side = 1, line = 2.5, font = 2, text = 'Free-gonad Weight [g]')
mtext(side = 2, line = 2.5, font = 2, text = 'Total Weight [g]')
box()
lines(mod1X, mod1Y, col = 'red')
legend('topleft', bty = 'n', text.font = 2,
       legend = c('Y = a + bX', paste('a =', round(cof[1],2)), paste('b =', round(cof[2],2))))

# Plot de Peso Total vs Peso de Gonada
plot(lab[,1], lab[,3], xlab = '', ylab = '', axes = F)
axis(1, font = 2, lwd.ticks = 2)
axis(2, font = 2, lwd.ticks = 2, las = 2)
mtext(side = 1, line = 2.5, font = 2, text = 'Total Weight [g]')
mtext(side = 2, line = 2.5, font = 2, text = 'Gonad Weight [g]')
box()

# Plot de Peso Total vs GI
plot(lab[,1], lab[,4], xlab = '', ylab = '', axes = F)
axis(1, font = 2, lwd.ticks = 2)
axis(2, font = 2, lwd.ticks = 2, las = 2)
mtext(side = 1, line = 2.5, font = 2, text = 'Total Weight [g]')
mtext(side = 2, line = 2.5, font = 2, text = 'Gonadosomatic Index [%]')
box()

# Plot de Peso Total vs Fecundidad Parcial
lab <- read.table(file = paste0(dirpath, 'Santander et al 1984 tabla3.csv'), header = T, sep = ';')
lab$PesoTotal <- cof[1] + cof[2] * lab[,2]
plot(lab[,3], lab[,1], xlab = '', ylab = '', axes = F)
axis(1, font = 2, lwd.ticks = 2)
axis(2, font = 2, lwd.ticks = 2)
mtext(side = 2, line = 2.5, font = 2, text = 'Partial Fecundity [#eggs/batch]')
mtext(side = 1, line = 2.5, font = 2, text = 'Total weight [g]')
box()

dev.off()
