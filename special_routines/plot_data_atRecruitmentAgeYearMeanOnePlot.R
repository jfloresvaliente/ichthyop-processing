#=============================================================================#
# Name   : plot_data_atRecruitmentAgeYearMeanOnePlot
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(ggplot2)
library(fields)
library(hexbin)
library(gridExtra)

dirpath  <- 'E:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj/out_case2/results/'
xlim     <- c(-85, -70) # Londitude
ylim     <- c(-20, 0)   # Latitude
bin_lon  <- .1
bin_lat  <- .1

#========== Zlim ==========#
zlimA <- c(40,90) # Age at recruitment
zlimB <- c(0,200) # No mortality
zlimC <- c(0,0.1) # Constant mortality

xposlab <- -82
yposlab <- -23.7
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

load(file = paste0(dirpath, 'data_atRecruitmentAge.Rdata'))

df <- subset(df, df$IfRecruited == 1)

# # Quitar los puntos mas extremos
# latilim       <- c(-20, -2)   # Latitude extension of the spawning zone
# lat_div       <- 0.1          # Latitudinal resolution
# 
# lat_ini <- seq(latilim[1], latilim[2], lat_div)
# lat_out <- lat_ini + lat_div
# 
# df2 <- NULL
# for(i in 1:(length(lat_ini)-1)){
#   lat_sub <- subset(df, df$Lat_end >= lat_ini[i] & df$Lat_end < lat_out[i])
#   lon_loin <- range(lat_sub$Lon_end)[1]
#   lat_sub <- subset(lat_sub, lat_sub$Lon_end >= (lon_loin+lat_div*4))
#   df2 <- rbind(df2, lat_sub)
# }
# df <- df2

rownames(df) <- NULL
#=============================================================================#
# stat_bin_hex: count number of particles by pixel
#=============================================================================#
ggname   <- paste0(dirpath, 'SpatialDistributionNumberIndividuals', bin_lat, '.png')
p1 <- ggplot(data = df)+
  stat_bin_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini), binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlimB, na.value = '#800000')+
  labs(x = 'Longitude', y = 'Latitude', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=25, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=25, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.8, 0.9),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap

png(filename = ggname, width = 750, height = 850, res = 120)
grid.arrange(p1, nrow = 1)

# png(filename = ggname, width = 950, height = 350, res = 120)
# grid.arrange(p3, p1, p5, nrow = 1)

dev.off()

#=============================================================================#
# stat_summary_hex: mean age recruited by pixel
#=============================================================================#
ggname   <- paste0(dirpath, 'SpatialDistributionAgeRecruitment', bin_lat, '.png')
p1 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = Age), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = zlimA, na.value = '#00008F')+
  labs(x = 'Longitude', y = 'Latitude', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=25, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=25, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.8, 0.9),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap

png(filename = ggname, width = 750, height = 850, res = 120)
grid.arrange(p1, nrow = 1)

# png(filename = ggname, width = 950, height = 350, res = 120)
# grid.arrange(p3, p1, p5, nrow = 1)

dev.off()

#=============================================================================#
# stat_summary_hex: mean N_constant by pixel
#=============================================================================#
ggname   <- paste0(dirpath, 'SpatialDistributionN_constant', bin_lat, '.png')
p1 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_constant), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlimC, na.value = '#800000')+
  labs(x = 'Longitude', y = 'Latitude', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=25, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=25, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.8, 0.9),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap

png(filename = ggname, width = 750, height = 850, res = 120)
grid.arrange(p1, nrow = 1)

# png(filename = ggname, width = 950, height = 350, res = 120)
# grid.arrange(p3, p1, p5, nrow = 1)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#