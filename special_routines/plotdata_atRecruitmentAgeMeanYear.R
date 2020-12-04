#=============================================================================#
# Name   : plotdata_atRecruitmentAgeMeanYear
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

dirpath  <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/DEB/out/results/'
xlim     <- c(-85, -70) # Londitude
ylim     <- c(-20, 0)   # Latitude
bin_lon  <- .25
bin_lat  <- .25
ggname   <- paste0(dirpath, 'grid_arrange0.25.png')
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

load(file = paste0(dirpath, 'data_atRecruitmentAge.Rdata'))

#=============================================================================#
# stat_bin_hex: count number of particles by pixel (0.25º)
#=============================================================================#
p1 <- ggplot(data = df)+
  stat_bin_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini), binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,1200), na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Count') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.85, 0.7),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p2 <- ggplot(data = df)+
  stat_bin_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end), binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,1200), na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Count') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.85, 0.7),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

#=============================================================================#
# stat_summary_hex: mean age recruited by pixel (0.25º)
#=============================================================================#
p3 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = AgeRecruited), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = c(15,50), na.value = '#00008F')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited\nAge') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.85, 0.7),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p4 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = AgeRecruited), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = c(15,50), na.value = '#00008F')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited\nAge') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.85, 0.7),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

#=============================================================================#
# stat_summary_hex: mean N_constant by pixel (0.25º)
#=============================================================================#
p5 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_constant), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,70), na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Constant\nMortality') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.85, 0.7),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p6 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = N_constant), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,70), na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Constant\nMortality') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.85, 0.7),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

#=============================================================================#
# stat_summary_hex: mean N_length by pixel (0.25º)
#=============================================================================#
p7 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_length), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,35), na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Length\nDependent\nMortality') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.85, 0.7),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p8 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = N_length), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,35), na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Length\nDependent\nMortality') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.85, 0.7),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

png(filename = ggname, width = 1250, height = 750, res = 120)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#