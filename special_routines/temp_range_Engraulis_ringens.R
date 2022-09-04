#=============================================================================#
# Name   : temp_range_Engraulis_ringens
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : calculate and plot the temperature correction curve of the DEB model
# URL    : 
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
dirpath  <- 'C:/Users/jflores/Desktop/'
out_name <- 'simu'

T_K    <- 273.15;     # Kelvin
T_ref  <- 16 + T_K;   # Kelvin
Temp   <- seq(from = 0, to = 35, by = 0.01) + T_K; # Kelvin

# Parameters
T_L  <- 6 + T_K     # K Lower temp boundary
T_H  <- 21 + T_K    # K Upper temp boundary
T_A  <- 9800        # K Arrhenius temperature
T_AL <- 20000       # K Arrh. temp for lower boundary
T_AH <- 190000/2    # K Arrh. temp for upper boundary

s_A = exp(T_A/T_ref - T_A/Temp)  # Arrhenius factor

# 1-parameter correction factor
TC_1 = s_A

# 5-parameter correction factor
# if(T_L > T_ref || T_H < T_ref) warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')

s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/Temp - T_AL/T_L))
s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/Temp))
TC_5      <- s_A * ((Temp <= T_ref) * s_L_ratio + (Temp > T_ref) * s_H_ratio)

# figures
png(filename = paste0(dirpath, out_name, '.png'), width = 1250, height = 850, res = 120)
par(mar = c(5.5,5.5,1,1))
plot(Temp - T_K, TC_1, type = 'n', axes = F, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i', xlim = c(0,35), ylim = c(0,2.5))
axis(1, font = 2, lwd.ticks = 2, cex.axis = 1.5)
axis(2, font = 2, lwd.ticks = 2, cex.axis = 1.5, las = 2)
box(lwd = 2)

# lines(Temp - T_K, TC_1, lwd = 4)
lines(Temp - T_K, TC_5, lwd = 5, col = 'red')

mtext(side = 1, line = 3.5, font = 2, cex = 1.6, text = 'Temprature (ºC)')
mtext(side = 2, line = 3.5, font = 2, cex = 1.6, text = 'Correction factor')

# text(2.5, 7.5, adj = 0, font = 2, bquote(paste('T'[L]*' = ',  .(T_L-T_K))))
# text(2.5, 6.8, adj = 0, font = 2, bquote(paste('T'[H]*' = ',  .(T_H-T_K))))
# text(2.5, 6.1, adj = 0, font = 2, bquote(paste('T'[A]*' = ',  .(T_A))))
# text(2.5, 5.4, adj = 0, font = 2, bquote(paste('T'[AL]*' = ', .(T_AL))))
# text(2.5, 4.7, adj = 0, font = 2, bquote(paste('T'[AH]*' = ', .(T_AH))))
# text(2.5, 4.0, adj = 0, font = 2, bquote(paste('TC'[max]*' = ', .(Temp[which.max(TC_5)]-T_K))))

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#