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

dirpath1  <- 'C:/Users/jflores/Desktop/'
dirpath2  <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB/k_x1.6/out/results/'
xlim     <- c(-85, -70) # Londitude
ylim     <- c(-20, 0)   # Latitude
bin1      <- .25
bin2      <- .1
ggname   <- paste0(dirpath1, 'grid_arrange', bin2, '.png')

zlim1 <- c(0, 7000)
zlim2 <- c(0, 4000)
zlim3 <- c(0, 2)
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
df1 <- read.table(file = paste0(dirpath1, 'Climatology_Anc_egg.csv'), header = T, sep = ';')
df1$lon <- df1$lon - 360

df2 <- read.table(file = paste0(dirpath1, 'Climatology_Anc_lar.csv'), header = T, sep = ';')
df2$lon <- df2$lon - 360

load(file = paste0(dirpath2, 'data_atRecruitmentAge.Rdata'))

# Quitar los puntos mas extremos
latilim       <- c(-20, -2)   # Latitude extension of the spawning zone
lat_div       <- 0.1          # Latitudinal resolution

lat_ini <- seq(latilim[1], latilim[2], lat_div)
lat_out <- lat_ini + lat_div

df3 <- NULL
for(i in 1:(length(lat_ini)-1)){
  lat_sub <- subset(df, df$Lat_end >= lat_ini[i] & df$Lat_end < lat_out[i])
  lon_loin <- range(lat_sub$Lon_end)[1]
  lat_sub <- subset(lat_sub, lat_sub$Lon_end >= (lon_loin+lat_div*4))
  df3 <- rbind(df3, lat_sub)
}
df <- df3
rownames(df) <- NULL

#=============================================================================#
# stat_summary_hex: mean #egg & # larvae
#=============================================================================#
p1 <- ggplot(data = df1)+
  stat_summary_hex(data = df1, mapping = aes(x = lon, y = lat, z = Anc_egg), fun = sum, binwidth = c(bin1, bin1))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim1, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = -74.5, y = -0.3, color = 'black', size = 3, hjust = 0.5, label = '# Eggs')+
  # annotate(geom='text', x = -84.5, y = -0.3, color = 'black', size = 4, hjust = 0.5, label = 'e)')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.8, 0.65),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p2 <- ggplot(data = df2)+
  stat_summary_hex(data = df2, mapping = aes(x = lon, y = lat, z = Anc_lar), fun = sum, binwidth = c(bin1, bin1))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim2, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = -74.5, y = -0.3, color = 'black', size = 3, hjust = 0.5, label = '# Larvae')+
  # annotate(geom='text', x = -84.5, y = -0.3, color = 'black', size = 4, hjust = 0.5, label = 'f)')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.8, 0.65),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p3 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_constant), fun = sum, binwidth = c(bin2, bin2))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim3, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = -74.5, y = -0.3, color = 'black', size = 3, hjust = 0.5, label = 'Constant\nMortality')+
  # annotate(geom='text', x = -84.5, y = -0.3, color = 'black', size = 4, hjust = 0.5, label = 'e)')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.8, 0.65),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

png(filename = ggname, width = 950, height = 750, res = 120)
# grid.arrange(p3, p1, p5, p7, p4, p2, p6, p8, nrow = 2)
grid.arrange(p1, p2, p3, nrow = 1)
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#