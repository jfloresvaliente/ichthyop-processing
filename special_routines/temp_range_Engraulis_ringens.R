#=============================================================================#
# Temperature correction factor for E. ringens to be used in Ichthyop-DEB
# from tempcorr.m from DEBTool
#
# Comparison with temperature function of Xu et al. 2013. was not possible
# as we did not find the equation
# it must be in Fish Bioenergetics 4.0. 
# https://www.tandfonline.com/doi/pdf/10.1080/03632415.2017.1377558
# http://fishbioenergetics.org/
  # but not easy to find
#
# 21/12/2021 Jorge Flores ; Laure Pecquerie
#=============================================================================#

T_K    <- 273.15;     # Kelvin
T_ref  <- 16 + T_K;   # Kelvin
T_hot  <- 25 # Tempertura letal
T_cold <- 3 # Temepratura letal
# Temperature range
Temp   <- seq(from = 0, to = 35, by = 0.01) + T_K; # Kelvin

# Parameters
T_L  <- 6 + T_K     # K Lower temp boundary
T_H  <- 22 + T_K    # K Upper temp boundary
T_A  <- 9800        # K Arrhenius temperature
T_AL <- 20000       # K Arrh. temp for lower boundary
T_AH <- 190000/4    # K Arrh. temp for upper boundary

s_A = exp(T_A/T_ref - T_A/Temp)  # Arrhenius factor

# 1-parameter correction factor
TC_1 = s_A

# para agregar temperaturas letales
TC_1[Temp <= T_cold + T_K] <- NA
TC_1[Temp >= T_hot + T_K] <- NA

cold_ind <- which(Temp == T_cold + T_K)
hot_ind  <- which(Temp == T_hot + T_K)

# 5-parameter correction factor
if(T_L > T_ref || T_H < T_ref){
  warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')
}

s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/Temp - T_AL/T_L))
s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/Temp))
TC_5      <- s_A * ((Temp <= T_ref) * s_L_ratio + (Temp > T_ref) * s_H_ratio)

# Parameter names for the plot
par1 <- paste0('T_L = ', (T_L - T_K))
par2 <- paste0('T_H = ', (T_H - T_K))
par3 <- paste0('T_A = ', T_A)
par4 <- paste0('T_A_L = ', round(T_AL))
par5 <- paste0('T_A_H = ', round(T_AH))

# figures
png(filename = 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/TCorr/cfg.png', width = 850, height = 850, res = 120)
par(mar = c(3.5,3.5,1.5,1))
plot(Temp - T_K, TC_1, type = 'n', axes = F, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i', xlim = c(0,35), ylim = c(0,8.5))
axis(1, font = 2, lwd.ticks = 2)
axis(2, font = 2, lwd.ticks = 2, las = 2)
box(lwd = 2)

lines(Temp - T_K, TC_1, lwd = 2)
# lines(Temp - T_K, TC_5, lwd = 2, col = 'red')

segments(x0 = Temp[cold_ind]-T_K, y0 = 0, x1 = Temp[cold_ind]-T_K, y1 = TC_1[cold_ind+1], lwd = 2)
segments(x0 = Temp[hot_ind]-T_K,  y0 = 0, x1 = Temp[hot_ind]-T_K,  y1 = TC_1[hot_ind-1],  lwd = 2)

mtext(side = 3, line = 0, font = 2, cex = 1.2, text = 'Temperature correction factor', adj = 0)
mtext(side = 1, line = 2, font = 2, cex = 1.2, text = 'Temprature (ºC)')
mtext(side = 2, line = 2, font = 2, cex = 1.2, text = 'Correction factor TC (#)')

# hot_temp <- TC_1[which(Temp == T_hot + T_K)]
# segments(x0 = T_hot, y0 = 0, x1 = T_hot, y1 = hot_temp, col = 'red', lty = 1, lwd = 2)

# cold_temp <- TC_1[which(Temp == T_cold + T_K)]
# segments(x0 = T_cold, y0 = 0, x1 = T_cold, y1 = cold_temp, col = 'red', lty = 1, lwd = 2)

# text(2.5, 7.5, par1, adj = 0, font = 2)
# text(2.5, 6.5, par2, adj = 0, font = 2)
text(2.5, 5.5, par3, adj = 0, font = 2)
# text(2.5, 4.5, par4, adj = 0, font = 2)
# text(2.5, 3.5, par5, adj = 0, font = 2)

dev.off()
