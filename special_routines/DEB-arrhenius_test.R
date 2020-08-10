# dat  <- read.table('C:/Users/jflores/Desktop/filename.txt', header = F, sep = ',')
# dat2 <-read.table('C:/Users/jflores/Desktop/CastroHernandesY1995.csv', header = F, sep = ';')
# X <- seq(0,40,0.1)
# Yline <- 2.9 + (0.68*X)
# 
# x11()
# plot(dat[,1], dat[,2]*10, type = 'l', axes = F, lwd = 2,
#      xlim = c(0,40), ylim = c(0,25),
#      xlab = '', ylab = '',
#      yaxs = 'i', xaxs = 'i')
# axis(1)
# axis(2)
# mtext(side = 1, line = 2, font = 2, text = 'Age in Days')
# mtext(side = 2, line = 2, font = 2, text = 'Physical length (mm)')
# 
# #CastroHernandez points and lines
# points(dat2[,1], dat2[,2], col = 'red', pch = 19)
# lines(X, Yline, col = 'red', lwd = 2)
# 
# legend('topleft', legend = c('DEB', '1995'), col = c('black', 'red'), lty = 1, bty = 'n', lwd = 2)




dat <- read.table('C:/Users/jflores/Desktop/cuatroyears.txt', header = F, sep = ',')
dat <- dat[seq(1, dim(dat)[1], 5),]

# VonBertalanffy
t0 <- -0.14
Linf <- 20.5
k <- 0.86

t <- seq(1, 4*365, 1)/365
L <- Linf*(1- exp(-k*(t - t0)))

x11()
plot(dat[,1]/365, dat[,2], type = 'l', axes = F, lwd = 2,
     xlim = c(0,4), ylim = c(0,25),
     xlab = '', ylab = '',
     yaxs = 'i', xaxs = 'i')
axis(1)
axis(2)
mtext(side = 1, line = 2, font = 2, text = 'Time in Years')
mtext(side = 2, line = 2, font = 2, text = 'Physical length (mm)')

lines(t, L, col = 'red', lwd = 2)

legend('topleft', legend = c('DEB', 'von Bertalanffy'), col = c('black', 'red'), lty = 1, bty = 'n', lwd = 2)



# # Temperaturas en Cº
# temp  <- seq(10, 25, .1)
# Tref  <- 16
# 
# # Temperatura de Arrhenius
# TA    <- seq(15800, 3800, -3000)
# 
# #======== No cambiar nada desde aqui ========#
# # Temperaturas en Kº
# tempK <- temp + 273
# 
# # cT funcion
# cT <- function(TA = TA, ref = 20+273, temperatureK = NULL){
#   c_T <- exp( (TA/ref) - (TA/temperatureK))
#   return(c_T)
# }
# 
# ct_cal <- NULL
# for(i in 1:length(TA)){
#   Trefk <- Tref[1] + 273
#   m <- cT(TA = TA[i], ref = Trefk, temperatureK = tempK)
#   ct_cal <- cbind(ct_cal, m)
# }
# 
# x11()
# 
# par(mfrow = c(2,2), mar = c(4,4,.5,.5))
# 
# plot(temp, ct_cal[,1],type = 'n', xlab = '', ylab = '')
# mtext(side = 1, line = 2, text = 'Temperature Cº')
# mtext(side = 2, line = 2, text = 'cT')
# col <- 1:length(TA)
# for(i in 1:length(TA)){
#   lines(temp, ct_cal[,i], col = col[i])
# }
# # legend('topleft', legend = paste(TA, 'ºK'),
# #        col = col, lty = 1, bty = 'n', title = 'Arrhenius Temperature')
# 
# pMref <- 48
# pM <- pMref * ct_cal
# 
# # x11()
# plot(temp, pM[,1],type = 'n', xlab = '', ylab = '')
# mtext(side = 1, line = 2, text = 'Temperature Cº')
# mtext(side = 2, line = 2, text = 'Mantenance Costs [J cm???3 d???1]')
# col <- 1:length(TA)
# for(i in 1:length(TA)){
#   lines(temp, pM[,i], col = col[i])
# }
# # legend('topleft', legend = paste(TA, 'ºK'),
# #        col = col, lty = 1, bty = 'n', title = 'Arrhenius Temperature')
# 
# 
# 
# pXMref <- 325
# ae <- 0.71
# pxM <- pXMref * ae * ct_cal
# 
# # x11()
# plot(temp, pxM[,1],type = 'n', xlab = '', ylab = '')
# mtext(side = 1, line = 2, text = 'Temperature Cº')
# mtext(side = 2, line = 2, text = 'Maximum Assimilation [J cm???2 d???1]')
# col <- 1:length(TA)
# for(i in 1:length(TA)){
#   lines(temp, pxM[,i], col = col[i])
# }
# # legend('topleft', legend = paste(TA, 'ºK'),
# #        col = col, lty = 1, bty = 'n', title = 'Arrhenius Temperature')
# 
# plot(1, type = 'n',axes = F, xlab = '', ylab = '')
# legend('topleft', legend = paste(TA, 'ºK'),
#        col = col, lty = 1, bty = 'n', title = 'Arrhenius Temperature')
# 
# 
# 
# # # Temperaturas en Cº
# # temp  <- seq(0, 35, 0.1)
# # Tref  <- c(16, 20, 24, 28)
# # 
# # # Temperatura de Arrhenius
# # TA    <- 9800
# # 
# # #======== No cambiar nada desde aqui ========#
# # # Temperaturas en Kº
# # tempK <- temp + 273
# # 
# # # cT funcion
# # cT <- function(TA = TA, ref = 20+273, temperatureK = NULL){
# #   c_T <- exp( (TA/ref) - (TA/temperatureK))
# #   return(c_T)
# # }
# # 
# # ct_cal <- NULL
# # for(i in 1:length(Tref)){
# #   Trefk <- Tref[i] + 273
# #   m <- cT(TA = TA, ref = Trefk, temperatureK = tempK)
# #   ct_cal <- cbind(ct_cal, m)
# # }
# # 
# # x11()
# # plot(temp, ct_cal[,1],type = 'n', xlab = 'Temperature Cº', ylab = 'cT')
# # col <- 1:length(Tref)
# # for(i in 1:length(Tref)){
# #   lines(temp, ct_cal[,i], col = col[i])
# # }
# # legend('topleft', legend = paste(Tref, 'ºC'), col = col, lty = 1, bty = 'n', title = 'Reference Temperature,')
