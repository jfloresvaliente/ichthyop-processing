# Plot de simus DEB-matlab (parametros anchoveta del mediterraneo) vs VonBertalanffy (Marzloff et al 2009)
dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/'

dat <- read.table(paste0(dirpath, '/DEB_4years_vari_Temp.txt'), header = F, sep = ',')
colnames(dat) <- c('t', 10:20)
# dat <- dat[seq(1,dim(dat)[1],100),]
t_y <- dat[,1]/365
t_d <- dat[,1]
dat <- dat[,-c(1)]

#Internal function to calculate error bars
error_bar <- function(x, a = 0.05){
  # x = vector or matrix with data to evaluate
  
  if(!is.null(a)){
    n  <- length(x)
    m  <- mean(x, na.rm = T)
    tt <- -qt(p = a/2, df = n-1)
    ee <- sd(x)/sqrt(n)  # standard error
    e  <- tt*ee          # error range
    d  <- e/m            # relative error, says that the confidence interval is a percentage of the value
    li <- m-e            # lower limit
    ls <- m+e            # upper limit
    stat <- c(m, li, ls)
  }else{
    n  <- length(x)
    m  <- mean(x, na.rm = T)
    tt <- -qt(p = a/2, df = n-1)
    ee <- sd(x)/sqrt(n)  # standard error
    li <- m-ee           # lower limit
    ls <- m+ee           # upper limit
    stat <- c(m, li, ls)
  }
  return(stat)
}

m <- apply(X = dat, MARGIN = 1, FUN = error_bar)

# VonBertalanffy
t0   <- -0.14
Linf <- 20.5
k    <- 0.86
t_VB <- seq(1, 4*365, 1)/365
L    <- Linf*(1- exp(-k*(t_VB - t0)))
VB   <- data.frame(t_VB,L)

#----------- plots -----------#
# Plot 4 years: DEB vs VonBertalanffy
png(filename = paste0(dirpath,'/DEB_VB_4years.png'), width = 850, height = 850, res = 120)
par(lwd = 2)
plot(t_y, m[1,], type = 'n', xlab = '', ylab = '', xlim = c(0,4), ylim = c(0,22), axes = F)
axis(1, font = 2, lwd = 2)
axis(2, font = 2, lwd = 2)
polygon(x = c(t_y, rev(t_y)), y = c(m[2,], rev(m[3,])), border = NA, col = 'grey80')
lines(t_y, m[1,], lwd = 2)
lines(t_y,dat[,1] , lty = 2, col = 'grey80')
lines(t_y,dat[,11], lty = 2, col = 'grey80')
lines(t_VB, L, col = 'red', lwd = 2)
mtext(side = 1, line = 2, font = 2, text = 'Time in years')
mtext(side = 2, line = 2, font = 2, text = 'Length (cm)')
legend('topleft', legend = c('Von Bertalanffy', 'DEB'), bty = 'n', col = c('red', 'black'), lty = 1, lwd = 2, text.font = 2, text.col = c('red', 'black'))
box()
dev.off()


# subset a 40 dias
day    <- 40
in_day <- max(which(t_d < day))
in_VB  <- max(which(t_VB*365 <= day))

dat    <- dat[1:in_day,]
m      <- m[,1:in_day]
t_y    <- t_y[1:in_day]
t_d    <- t_d[1:in_day]
t_VB   <- t_VB[1:in_VB]
L      <- L[1:in_VB]

# Plot 40 days: DEB vs VonBertalanffy
png(filename = paste0(dirpath, '/DEB_VB_40days.png'), width = 850, height = 850, res = 120)
par(lwd = 2)
plot(t_d, m[1,], type = 'n', xlab = '', ylab = '', xlim = c(0,day), ylim = c(0,5), axes = F)
axis(1, font = 2, lwd = 2)
axis(2, font = 2, lwd = 2)
polygon(x = c(t_d, rev(t_d)), y = c(m[2,], rev(m[3,])), border = NA, col = 'grey80')
lines(t_d, m[1,], lwd = 2)
lines(t_d,dat[,1] , lty = 2, col = 'grey80')
lines(t_d,dat[,11], lty = 2, col = 'grey80')
lines(t_VB*365, L, col = 'red', lwd = 2)
mtext(side = 1, line = 2, font = 2, text = 'Time in days')
mtext(side = 2, line = 2, font = 2, text = 'Length (cm)')
legend('topleft', legend = c('Von Bertalanffy', 'DEB'), bty = 'n', col = c('red', 'black'), lty = 1, lwd = 2, text.font = 2, text.col = c('red', 'black'))
box()
dev.off()

