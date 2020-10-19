dirpath <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/MESO60dias/results/'

# x11()
# par(mfrow = c(2,4))
temp <- NULL
for(i in 1:12){
  rfile <- paste0(dirpath, 'trajectoriesM',i,'.Rdata')
  print(rfile)
  load(file = rfile)
  temp <- c(temp, trajectories$temp)
  # hist(trajectories$temp, xlim = c(0,35), ylim = c(0,1e6))
}

#Internal function to calculate error bars
error_bar <- function(x, a = 0.05){
  # x = vector or matrix with dat1a to evaluate

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

error_bar(x = temp)
range(temp)
hist(temp, freq = F, breaks = c(0:30))
sd(temp)
mean(temp)
#


# # Plot de simus DEB-matlab (parametros anchoveta del mediterraneo) vs VonBertalanffy (Marzloff et al 2009)
# dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/'
# 
# dat1 <- read.table(paste0(dirpath, '/DEB_4years_vari_Temp.txt'), header = F, sep = ',')
# dat2 <- read.table('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/engraulis_data/Crecimiento_E.ringens.csv', header = T, sep = ',')
# 
# #========== Preparar dat1 ==========#
# colnames(dat1) <- c('t', 10:20)
# dat1 <- dat1[seq(1,dim(dat1)[1],100), ]
# dat1 <- subset(dat1, dat1$t <= 30)
# t_y1 <- dat1[,1]/365
# t_d1 <- dat1[,1]
# dat1 <- dat1[,-c(1)]
# 
# 
# #========== Preparar dat2 ==========#
# t_y2 <- dat2$t/365
# t_d2 <- dat2$t
# # dat2$L <- dat2$L/10 # transformar de mm a cm
# 
# 
# m <- apply(X = dat1, MARGIN = 1, FUN = error_bar)
# 
# plot(t_d1, m[1,], type = 'n', xlab = '', ylab = '', xlim = c(0,30), ylim = c(0,3), axes = F)
# axis(1, font = 2, lwd = 2)
# axis(2, font = 2, lwd = 2)
# polygon(x = c(t_d1, rev(t_d1)), y = c(m[2,], rev(m[3,])), border = NA, col = 'grey80')
# lines(t_d1, m[1,], lwd = 2)
# lines(t_d1,dat1[,1] , lty = 2, col = 'grey80')
# lines(t_d1,dat1[,11], lty = 2, col = 'grey80')
# 


#----------- plots -----------#
# Plot 4 years: DEB vs VonBertalanffy
# png(filename = paste0(dirpath,'/DEB_VB_4years.png'), width = 850, height = 850, res = 120)
# par(lwd = 2)
# plot(t_y1, m[1,], type = 'n', xlab = '', ylab = '', xlim = c(0,4), ylim = c(0,22), axes = F)
# axis(1, font = 2, lwd = 2)
# axis(2, font = 2, lwd = 2)
# polygon(x = c(t_y1, rev(t_y1)), y = c(m[2,], rev(m[3,])), border = NA, col = 'grey80')
# lines(t_y1, m[1,], lwd = 2)
# lines(t_y1,dat1[,1] , lty = 2, col = 'grey80')
# lines(t_y1,dat1[,11], lty = 2, col = 'grey80')
# 
# points(dat2$t/365, dat2$L/10, pch = 19)
# 
# mtext(side = 1, line = 2, font = 2, text = 'Time in years')
# mtext(side = 2, line = 2, font = 2, text = 'Length (cm)')
# legend('topleft', legend = c('Von Bertalanffy', 'DEB'), bty = 'n', col = c('red', 'black'), lty = 1, lwd = 2, text.font = 2, text.col = c('red', 'black'))
# box()
# dev.off()

# p <- ggplot(data = dat2, mapping = aes(x = t, y = L, color = Autor)) +
#   geom_point()
# p

#
# plot(dat2$t, dat2$L, type = 'n')


#









# # subset a 40 dias
# day    <- 40
# in_day <- max(which(t_d1 < day))
# 
# 
# dat1    <- dat1[1:in_day,]
# m      <- m[,1:in_day]
# t_y1    <- t_y1[1:in_day]
# t_d1    <- t_d1[1:in_day]
# 
# # Plot 40 days: DEB vs VonBertalanffy
# # png(filename = paste0(dirpath, '/DEB_VB_40days.png'), width = 850, height = 850, res = 120)
# par(lwd = 2)
# plot(t_d1, m[1,], type = 'n', xlab = '', ylab = '', xlim = c(0,day), ylim = c(0,5), axes = F)
# axis(1, font = 2, lwd = 2)
# axis(2, font = 2, lwd = 2)
# polygon(x = c(t_d1, rev(t_d1)), y = c(m[2,], rev(m[3,])), border = NA, col = 'grey80')
# lines(t_d1, m[1,], lwd = 2)
# lines(t_d1,dat1[,1] , lty = 2, col = 'grey80')
# lines(t_d1,dat1[,11], lty = 2, col = 'grey80')
# 
# mtext(side = 1, line = 2, font = 2, text = 'Time in days')
# mtext(side = 2, line = 2, font = 2, text = 'Length (cm)')
# # legend('topleft', legend = c('Von Bertalanffy', 'DEB'), bty = 'n', col = c('red', 'black'), lty = 1, lwd = 2, text.font = 2, text.col = c('red', 'black'))
# box()
# dev.off()
# 
# 
# 
# castro2000 <- subset(dat2, dat2$Autor == 'Castro2000')

library(ncdf4)
nc <- nc_open('C:/Users/jflores/Desktop/ichthyop-3.2_src/dist/output/compilado_ichthyop-run202008231133_s1.nc')

recruited <- ncvar_get(nc, 'recruited_zone')
length <- ncvar_get(nc, 'length')

# MESO , temp

