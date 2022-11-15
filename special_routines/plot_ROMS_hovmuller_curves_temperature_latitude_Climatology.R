#=============================================================================#
# Name   : plot_ROMS_hovmuller_curves_temperature_latitude_Climatology
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')

dirpath   <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/interpolatedYearMonth/'
sufijo    <- 'release_zone'
nlevels   <- 64 # Number of levels in the color palette
z_depth   <- -45 # debe ser un numero negativo
years <- 1:3

#===== Config for temp var =====#
namevar  <- 'TEMP'
zlim     <- c(.6, 2)
isolines <- seq(zlim[1], zlim[2], .2) # Isolines to be plotted
caption  <- 'Correction Temperature'

T_K    <- 273.15;     # Kelvin
T_ref  <- 16 + T_K;   # Kelvin
T_A    <- 10000
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
Rdata    <- paste0(dirpath, sufijo, '/',sufijo,'_', namevar, '_hovmuller_latitude',0,z_depth,'m.Rdata')
load(Rdata)

z <- hovmuller$z + T_K
y <- hovmuller$y
new_x <- dim(z)[1]/length(years)
x <- seq(from = 1, to = 12.999, length.out = new_x)

z2 <- array(data = NA, dim = c(new_x, dim(z)[2], length(years)))
ind_in <- seq(from = 1, length.out = length(years), by = new_x)
ind_on <- seq(from = new_x, length.out = length(years), by = new_x)
for(i in years){
  z2[,,i] <- z[ind_in[i] : ind_on[i],]
}
z <- apply(z2, c(1,2), mean, na.rm = T)


lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels) # Niveles para la paleta de color
ytics <- seq(from = range(y)[1], to = range(y)[2], by = 2)
ylabs <- paste0(abs(ytics), 'ºS')

#===== CURVA 1 =====#
# Parameters
T_L  <- 6 + T_K     # K Lower temp boundary
T_H  <- 21 + T_K    # K Upper temp boundary
T_A  <- T_A        # K Arrhenius temperature
T_AL <- 20000       # K Arrh. temp for lower boundary
T_AH <- 190000/2    # K Arrh. temp for upper boundary

s_A = exp(T_A/T_ref - T_A/z)  # Arrhenius factor

# 1-parameter correction factor
TC_1 = s_A

# 5-parameter correction factor
if(T_L > T_ref || T_H < T_ref){
  warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')
}

s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/z - T_AL/T_L))
s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/z))
TC_5      <- s_A * ((z <= T_ref) * s_L_ratio + (z > T_ref) * s_H_ratio)
z1 <- TC_5

#===== CURVA 2 =====#
# Parameters
T_L  <- 6 + T_K     # K Lower temp boundary
T_H  <- 24 + T_K    # K Upper temp boundary
T_A  <- T_A        # K Arrhenius temperature
T_AL <- 20000       # K Arrh. temp for lower boundary
T_AH <- 190000*3    # K Arrh. temp for upper boundary

s_A = exp(T_A/T_ref - T_A/z)  # Arrhenius factor

# 1-parameter correction factor
TC_1 = s_A

# 5-parameter correction factor
if(T_L > T_ref || T_H < T_ref){
  warning('Warning : invalid parameter combination, T_L > T_ref and/or T_H < T_ref\n')
}

s_L_ratio <- (1 + exp(T_AL/T_ref - T_AL/T_L)) / (1 + exp(T_AL/z - T_AL/T_L))
s_H_ratio <- (1 + exp(T_AH/T_H - T_AH/T_ref)) / (1 + exp(T_AH/T_H - T_AH/z))
TC_5      <- s_A * ((z <= T_ref) * s_L_ratio + (z > T_ref) * s_H_ratio)
z2 <- TC_5

# Make plots

# Case 1
png_name <- paste0(dirpath, sufijo, '/',sufijo,'_', namevar, '_hovmuller_latitude',0,z_depth,'mCase1_Climatology.png')
png(filename = png_name, width = 1850, height = 750, res = 120)
par(mar = c(5, 5, 3.5, 3.5))
filled.contour(x = x, y = y, z = z1, zlim = zlim,
               # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = '', ylab = '',
               plot.axes = {
                 contour(x = x, y = y, z = z1, levels = isolines, labels = isolines, add = T, lwd = 2, labcex = 1)
                 axis(side = 1, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                 axis(side = 2, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = ytics, labels = ylabs)
                 box(lwd = 2)
               },
               key.axes = axis(4, isolines, font = 2, lwd.ticks = 2, cex.axis = 1.5)
)
mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.45, text = 'Months')
mtext(side = 2, line = 3.8, font = 2, cex = 1.5, text = 'Latitude')
mtext(side = 3, line = 0.2, font = 2, cex = 1.5, adj = 0.00, text = caption)

dev.off()

# Case 2
png_name <- paste0(dirpath, sufijo, '/',sufijo,'_', namevar, '_hovmuller_latitude',0,z_depth,'mCase2_Climatology.png')
png(filename = png_name, width = 1850, height = 750, res = 120)
par(mar = c(5, 5, 3.5, 3.5))
filled.contour(x = x, y = y, z = z2, zlim = zlim,
               # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = '', ylab = '',
               plot.axes = {
                 contour(x = x, y = y, z = z2, levels = isolines, labels = isolines, add = T, lwd = 2, labcex = 1)
                 axis(side = 1, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                 axis(side = 2, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = ytics, labels = ylabs)
                 box(lwd = 2)
               },
               key.axes = axis(4, isolines, font = 2, lwd.ticks = 2, cex.axis = 1.5)
)
mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.45, text = 'Months')
mtext(side = 2, line = 3.8, font = 2, cex = 1.5, text = 'Latitude')
mtext(side = 3, line = 0.2, font = 2, cex = 1.5, adj = 0.00, text = caption)

dev.off()


print(range(z1))
print(range(z2))
# hcl.pals() # Funcion para listar la paleta de color disponible
#=============================================================================#
# END OF PROGRAM
#=============================================================================#