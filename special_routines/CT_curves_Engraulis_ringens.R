#=============================================================================#
# Name   : CT_curves_Engraulis_ringens
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Calculate and plot CT curve for DEB model
# URL    : 
#=============================================================================#
dirpath  <- 'C:/Users/jflores/Desktop/'
out_name <- 'E_ringens_CTcurves'

T_K    <- 273.15;     # Kelvin
T_ref  <- 16 + T_K;   # Kelvin
T_A    <- 10000

# Temperature range
Temp   <- seq(from = 0, to = 35, by = 0.01) + T_K; # Kelvin

text_size <- 1
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
png(filename = paste0(dirpath, out_name, '.png'), width = 850, height = 550, res = 120)
par(mar = c(4.5,5,1.5,1))
plot(Temp, type = 'n', axes = F, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i', xlim = c(0,35), ylim = c(0,3))
axis(1, font = 2, lwd.ticks = 2, cex = 2, cex.axis = 1.5)
axis(2, font = 2, lwd.ticks = 2, cex = 2, cex.axis = 1.5, las = 2)
box(lwd = 2)
mtext(side = 1, line = 2.5, font = 2, cex = 1.5, text = 'Temprature (ºC)')
mtext(side = 2, line = 3.5, font = 2, cex = 1.5, text = 'Correction factor')

#===== CURVA 1 =====#
# Parameters
T_L  <- 6 + T_K     # K Lower temp boundary
T_H  <- 24 + T_K    # K Upper temp boundary
T_A  <- T_A        # K Arrhenius temperature
T_AL <- 20000       # K Arrh. temp for lower boundary
T_AH <- 190000*3    # K Arrh. temp for upper boundary

s_A = exp(T_A/T_ref - T_A/Temp)  # Arrhenius factor

# 1-parameter correction factor
TC_1 = s_A

# 5-parameter correction factor
if(T_L > T_ref || T_H < T_ref){
  warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')
}

s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/Temp - T_AL/T_L))
s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/Temp))
TC_5      <- s_A * ((Temp <= T_ref) * s_L_ratio + (Temp > T_ref) * s_H_ratio)
TC_ring1 <- TC_5
lines(Temp - T_K, TC_5, lwd = 4.5, col = 'red')

# text(2.5, 2.8, adj = 0, font = 2, col = 'red', cex = text_size, bquote(paste('T'[L]*' = ',  .(T_L-T_K))))
# text(2.5, 2.5, adj = 0, font = 2, col = 'red', cex = text_size, bquote(paste('T'[H]*' = ',  .(T_H-T_K))))
# text(2.5, 2.2, adj = 0, font = 2, col = 'red', cex = text_size, bquote(paste('T'[A]*' = ',  .(T_A))))
# text(2.5, 1.9, adj = 0, font = 2, col = 'red', cex = text_size, bquote(paste('T'[AL]*' = ', .(T_AL))))
# text(2.5, 1.6, adj = 0, font = 2, col = 'red', cex = text_size, bquote(paste('T'[AH]*' = ', .(T_AH))))
# text(2.5, 1.3, adj = 0, font = 2, col = 'red', cex = text_size, bquote(paste('TC'[max]*' = ', .(Temp[which.max(TC_5)]-T_K))))

#===== CURVA 2 =====#
# Parameters
T_L  <- 6 + T_K     # K Lower temp boundary
T_H  <- 21 + T_K    # K Upper temp boundary
T_A  <- T_A        # K Arrhenius temperature
T_AL <- 20000       # K Arrh. temp for lower boundary
T_AH <- 190000/2    # K Arrh. temp for upper boundary

s_A = exp(T_A/T_ref - T_A/Temp)  # Arrhenius factor

# 1-parameter correction factor
TC_1 = s_A

# 5-parameter correction factor
if(T_L > T_ref || T_H < T_ref){
  warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')
}

s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/Temp - T_AL/T_L))
s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/Temp))
TC_5      <- s_A * ((Temp <= T_ref) * s_L_ratio + (Temp > T_ref) * s_H_ratio)
TC_ring2 <- TC_5
lines(Temp - T_K, TC_5, lwd = 3.0, col = 'blue')

# text(27, 2.8, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('T'[L]*' = ',  .(T_L-T_K))))
# text(27, 2.5, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('T'[H]*' = ',  .(T_H-T_K))))
# text(27, 2.2, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('T'[A]*' = ',  .(T_A))))
# text(27, 1.9, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('T'[AL]*' = ', .(T_AL))))
# text(27, 1.6, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('T'[AH]*' = ', .(T_AH))))
# text(27, 1.3, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('TC'[max]*' = ', .(Temp[which.max(TC_5)]-T_K))))

# #===== CURVE 3 =====#
# # Parameters
# T_L  <- 6 + T_K    # K Lower temp boundary
# T_H  <- 17 + T_K   # K Upper temp boundary
# T_A  <- T_A        # K Arrhenius temperature
# T_AL <- 20000      # K Arrh. temp for lower boundary
# T_AH <- 190000 / 4 # K Arrh. temp for upper boundary
# 
# s_A = exp(T_A/T_ref - T_A/Temp)  # Arrhenius factor
# 
# # 1-parameter correction factor
# TC_1 = s_A
# 
# # 5-parameter correction factor
# if(T_L > T_ref || T_H < T_ref){
#   warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')
# }
# 
# s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/Temp - T_AL/T_L))
# s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/Temp))
# TC_5      <- s_A * ((Temp <= T_ref) * s_L_ratio + (Temp > T_ref) * s_H_ratio)
# lines(Temp - T_K, TC_5, lwd = 2.0, col = 'green')
# 
# text(27, 2.8, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('T'[L]*' = ',  .(T_L-T_K))))
# text(27, 2.5, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('T'[H]*' = ',  .(T_H-T_K))))
# text(27, 2.2, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('T'[A]*' = ',  .(T_A))))
# text(27, 1.9, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('T'[AL]*' = ', .(T_AL))))
# text(27, 1.6, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('T'[AH]*' = ', .(T_AH))))
# text(27, 1.3, adj = 0, font = 2, col = 'blue', cex = text_size, bquote(paste('TC'[max]*' = ', .(Temp[which.max(TC_5)]-T_K))))

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#