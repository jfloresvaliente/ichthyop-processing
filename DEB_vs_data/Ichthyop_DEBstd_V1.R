#=============================================================================#
# Name   : Ichthyop_DEBstd_V1
# Author : Jorge Flores-Valiente
# Date   :
# Version:
# Aim    :
# URL    :
#=============================================================================#
library(ggplot2)
library(fields)

dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEBoutV1/'
L_wb    <- 0.25   # cm, total length at mouth opening --> ?? total or fork length? [add my pet]
L_wp    <- 9.077  # cm, total length at puberty --> ?? guess [add my pet]
del_M1  <- 0.154  # - , shape coefficient larvae (Total Length)[Pethybridge et al 2013]
del_M2  <- 0.169  # - , shape coefficient adult  (Total Length)[Pethybridge et al 2013]
xlim    <- c(0, 180)
ylim    <- c(0, 6)

# Compound parameters
V_b = (L_wb * del_M1)^3; # cm^3, structural volume at birth (first feeding)
V_p = (L_wp * del_M2)^3; # cm^3, structural volume at puberty

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dat  <- read.table(paste0(dirpath, 'DEB_out.txt'), header = T, sep = ',')
dat  <- dat[-dim(dat)[1],]
temp <- as.numeric(levels(factor(dat$temp)))

#------- Get physical length -------# TL = 1.141(SL)
dat$Lwt <- V^(1/3)/delta # For total length
dat$Lws <- dat$Lwt / 1.141    # For standard length

png(filename = paste0(dirpath, 'DEBoutV1_Lwt_Lws', '.png'), height = 850, width = 850, res = 120)
par(mfrow = c(2,2))
plot(t, Lw, type = 'l', xaxs = 'i', yaxs = 'i')
plot(t, Ww, type = 'l', xaxs = 'i', yaxs = 'i')
plot(Lw, Ww, type = 'l', xaxs = 'i', yaxs = 'i')
plot(t, Fec, type = 'b', xaxs = 'i', yaxs = 'i')
dev.off()

# #------- Get wet weight -------#
# d_V  <- 0.23   # g/cm^3, specific density of structure (dry weight)
# mu_V <- 500000 # J/mol, specific chemical potential of structure
# mu_E <- 550000 # J/mol, specific chemical potential of reserve
# w_V  <- 23.9   # g/mol, molecular weight of structure
# w_E  <- 23.9   # g/mol, molecular weight of reserve
# c_w  <- 0.756  # (c_w * W_w = total water weight)
# 
# W_V        <- d_V*V
# W_E        <- (w_E/mu_E)*E
# W_ER       <- (w_E/mu_E)*E_R
# Dry_weight <- cbind(W_V, W_E, W_ER) # g, Dry weight
# Wet_weight <- Dry_weight/(1 - c_w)  # g, Wet weight
# 
# Ww <- apply(Wet_weight, 1, sum) # g, Weight
# 
# #=============================== RESULTS =====================================#
# 
# #------ Age and length at transitions ------#
# i_b  <- which(V >= V_b)[1]
# i_b1 <- round(x = t[i_b] - t[1], digits = 4)
# i_b2 <- round(x = Lw[i_b], digits = 4)
# 
# # i_j  <- which(E_H >= E_Hj)[1]
# # i_j1 <- round(x = t[i_j] - t[1], digits = 4)
# # i_j2 <- round(x = Lw[i_j], digits = 4)
# 
# # i_y  <- which(E_H >= E_Hy)[1]
# # i_y1 <- round(t[i_y] - t[1], digits = 4)
# # i_y2 <- round(Lw[i_y], digits = 4)
# 
# i_p  <- which(V >= V_p)[1]
# i_p1 <- round(t[i_p] - t[1], digits = 4)
# i_p2 <- round(Lw[i_p], digits = 4)
# 
# txt_out <- rbind(
#   paste('age at birth =', i_b1, 'days'),
#   # paste('age at metamorphosis =', i_j1, 'days'),
#   # paste('age at early juvenile stage =', i_y1, 'days'),
#   paste('age at puberty =', i_p1, 'days'),
#   
#   paste('length at birth =', i_b2, 'cm'),
#   # paste('length at metamorphosis =', i_j2, 'cm'),
#   # paste('length at early juvenile stage =', i_y2, 'cm'),
#   paste('length at puberty =', i_p2, 'cm')
# )
# 
# write.table(x = txt_out, file = paste0(dirpath, 'DEB_out_',sufijo,'.txt'), row.names = F, col.names = F, append = T)

#------------------------ Plots ------------------------#
# 
# png(filename = paste0(dirpath, 'DEB_out_',sufijo,'1.png'), height = 850, width = 850, res = 120)
# par(mfrow = c(2,2))
# plot(t, E, type = 'l', xlab = 'Age in days (d)', ylab = 'Reserve E (J)')
# plot(t, V, type = 'l', xlab = 'Age in days (d)', ylab = 'Structure V (cm^3)')
# plot(t, type = 'n', xlab = 'Age in days (d)', ylab = 'E_H (J)')
# plot(t, E_R, type = 'l', xlab = 'Age in days (d)', ylab = 'E_R (J)')
# dev.off()
# 
# png(filename = paste0(dirpath, 'DEB_out_',sufijo,'2.png'), height = 850, width = 850, res = 120)
# par(mfrow = c(2,2))
# plot(t, Lw, type = 'l', xaxs = 'i', yaxs = 'i')
# plot(t, Ww, type = 'l', xaxs = 'i', yaxs = 'i')
# plot(Lw, Ww, type = 'l', xaxs = 'i', yaxs = 'i')
# plot(t, Fec, type = 'b', xaxs = 'i', yaxs = 'i')
# dev.off()

