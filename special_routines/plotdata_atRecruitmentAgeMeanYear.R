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

dirpath  <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/DEBf1/out/results/'
xlim     <- c(-85, -70) # Londitude
ylim     <- c(-20, 0)   # Latitude
bin_lon  <- .1
bin_lat  <- .1
ggname   <- paste0(dirpath, 'grid_arrange', bin_lat, '.png')

# #========== Simu f = 0.5 ==========#
# # zlim 0.1º
# zlim1 <- c(0,100)
# zlim2 <- c(30,60)
# zlim3 <- c(0,1)
# zlim4 <- c(0,0.5)
# 
# # zlim 0.25º
# zlim1 <- c(0,500)
# zlim2 <- c(15,50)
# zlim3 <- c(0,20)
# zlim4 <- c(0,10)

#========== Simu f = 1 & variable ==========#
# zlim 0.1º
zlim1 <- c(0,150)
zlim2 <- c(15,45)
zlim3 <- c(0,7.5)
zlim4 <- c(0,4)

# # zlim 0.25º
# zlim1 <- c(0,500)
# zlim2 <- c(15,50)
# zlim3 <- c(0,20)
# zlim4 <- c(0,10)

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

load(file = paste0(dirpath, 'data_atRecruitmentAge.Rdata'))

# Quitar los puntos mas extremos
latilim       <- c(-20, -2)   # Latitude extension of the spawning zone
lat_div       <- 0.1          # Latitudinal resolution

lat_ini <- seq(latilim[1], latilim[2], lat_div)
lat_out <- lat_ini + lat_div

df2 <- NULL
for(i in 1:(length(lat_ini)-1)){
  lat_sub <- subset(df, df$Lat_end >= lat_ini[i] & df$Lat_end < lat_out[i])
  lon_loin <- range(lat_sub$Lon_end)[1]
  lat_sub <- subset(lat_sub, lat_sub$Lon_end >= (lon_loin+lat_div*4))
  df2 <- rbind(df2, lat_sub)
}
df <- df2
rownames(df) <- NULL
#=============================================================================#
# stat_bin_hex: count number of particles by pixel (0.25º)
#=============================================================================#
p1 <- ggplot(data = df)+
  stat_bin_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini), binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim1, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = -74.5, y = -0.3, color = 'black', size = 3, hjust = 0.5, label = 'No mortality')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.8, 0.65),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p2 <- ggplot(data = df)+
  stat_bin_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end), binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim1, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = -74.5, y = -0.3, color = 'black', size = 3, hjust = 0.5, label = 'No mortality')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.8, 0.65),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

#=============================================================================#
# stat_summary_hex: mean age recruited by pixel (0.25º)
#=============================================================================#
p3 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = AgeRecruited), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = zlim2, na.value = '#00008F')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = -74.5, y = -0.3, color = 'black', size = 3, hjust = 0.5, label = 'Age at recruitment\n(days)')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.8, 0.65),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p4 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = AgeRecruited), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = zlim2, na.value = '#00008F')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = -74.5, y = -0.3, color = 'black', size = 3, hjust = 0.5, label = 'Age at recruitment\n(days)')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.8, 0.65),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

#=============================================================================#
# stat_summary_hex: mean N_constant by pixel (0.25º)
#=============================================================================#
p5 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_constant), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim3, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = -74.5, y = -0.3, color = 'black', size = 3, hjust = 0.5, label = 'Constant\nMortality')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.8, 0.65),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p6 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = N_constant), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim3, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = -74.5, y = -0.3, color = 'black', size = 3, hjust = 0.5, label = 'Constant\nMortality')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.8, 0.65),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

#=============================================================================#
# stat_summary_hex: mean N_length by pixel (0.25º)
#=============================================================================#
p7 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_length), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim4, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = -74.5, y = -0.3, color = 'black', size = 3, hjust = 0.5, label = 'Length-Dependent\nMortality')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.8, 0.65),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p8 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = N_length), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim4, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = -74.5, y = -0.3, color = 'black', size = 3, hjust = 0.5, label = 'Length-Dependent\nMortality')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.8, 0.65),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

png(filename = ggname, width = 1250, height = 750, res = 120)
grid.arrange(p1, p3, p5, p7, p2, p4, p6, p8, nrow = 2)
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#