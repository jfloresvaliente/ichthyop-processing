dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/'

dat <- as.matrix(read.table(paste0(dirpath, 'DEB_Lw.txt'), header = F, sep = ','))
dat <- dat[seq(1,dim(dat)[1],100),-c(1)]

# Relacion L/W de la literatura
a <- c(0.0215)
b <- c(2.6040)
model_source <- c('Villavicencio-Muck 1983')

dat_W <- a * dat^b

# Datos de Bibliografia
villa_muck <- matrix(data = c(0.4, 0.00015, 5, 1.4, 10, 8.7, 15, 24.9, 20, 52.6), byrow = T, ncol = 2)

dirpath <- 'C:/Users/jflores/Desktop/'
dat_nor <- as.matrix(read.table(paste0(dirpath, 'ChileNorte.txt'), header = F, sep = ''))
dat_nor <- exp(dat_nor)
dat_sud <- as.matrix(read.table(paste0(dirpath, 'ChileSur.txt'), header = F, sep = ''))
dat_sud <- exp(dat_sud)
dat_minano <- as.matrix(read.table(paste0(dirpath, 'Minano1968.csv'), header = T, sep = ';'))
dat_minano[,1] <- dat_minano[,1]/10

png('C:/Users/jflores/Desktop/LW_ringens.png', height = 850, width = 850, res = 120)
par(mar = c(4,4,2,2))

plot(as.vector(dat), as.vector(dat_W), type = 'n',
     xlim = c(0,25), ylim = c(0,70),
     xlab = '', ylab = '')

lines(as.vector(dat), as.vector(dat_W), type = 'p', col = 'red')

points(villa_muck[,1], villa_muck[,2], pch = 16)
points(dat_nor[,1], dat_nor[,2], pch = 1, cex = 0.7)
points(dat_sud[,1], dat_sud[,2], pch = 2, cex = 0.7)
points(dat_minano[,1], dat_minano[,2], pch = 3, cex = 0.7)

mtext(side = 1, line = 2.5, text = 'Length [cm]')
mtext(side = 2, line = 2.5, text = 'Wet weight [g]')

legend('topleft', bty = 'n',
       legend = c('DEB',
                  'Villavicencio-Muck 1985',
                  'Leal et al 2009 (Northern Chile)',
                  'Leal et al 2009 (Southern Chile)',
                  'Miñano 1968'),
       lwd = c(3,NA,NA,NA,NA),
       lty = c(1,NA,NA,NA,NA),
       pch = c(NA,16,1,2,3),
       col = c('red','black','black','black','black'))
legend('topright',
       legend = c('Length-Weight:',
                  model_source,
                  'W = aL^b',
                  paste('a = ', a),
                  paste('b = ', b)),
       bty = 'n')
dev.off()
