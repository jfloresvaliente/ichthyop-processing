#=============================================================================#
# Name   : plot_data_atRecruitmentAgeYearMean
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

dirpath  <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052/case1/results/'
xlim     <- c(-95, -70) # Londitude
ylim     <- c(-25, 0)   # Latitude
bin_lon  <- .1
bin_lat  <- .1
ggname   <- paste0(dirpath, 'grid_arrange', bin_lat, '.png')

#========== Zlim ==========#
zlimA <- c(20,90) # Age at recruitment
zlimB <- c(0,300) # No mortality
zlimC <- c(0,2) # Constant mortality

zlimD <- zlimA    # Age at recruitment
zlimE <- c(0,100) # No mortality
zlimF <- c(0,1) # Constant mortality

xposlab <- -85
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
p1 <- ggplot(data = df)+
  stat_bin_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini), binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlimB, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 3.5, hjust = 0, vjust = 1, label = 'atop(bold("b) Recruitment\'s Density"))', parse = TRUE)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.89, 0.75),
        legend.direction  = 'vertical',
        legend.key.height = unit(15, 'pt'),
        legend.key.width  = unit(05, 'pt'),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p2 <- ggplot(data = df)+
  stat_bin_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end), binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlimE, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 3.5, hjust = 0, vjust = 1, label = 'atop(bold("e) Recruitment\'s Density"))', parse = TRUE)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.89, 0.75),
        legend.direction  = 'vertical',
        legend.key.height = unit(15, 'pt'),
        legend.key.width  = unit(05, 'pt'),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

#=============================================================================#
# stat_summary_hex: mean age recruited by pixel
#=============================================================================#
p3 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = Age), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = zlimA, na.value = '#00008F')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 3.5, hjust = 0, vjust = 1, label = 'atop(bold("a) Age at recruitment (days)"))', parse = TRUE)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.89, 0.75),
        legend.direction  = 'vertical',
        legend.key.height = unit(15, 'pt'),
        legend.key.width  = unit(05, 'pt'),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p4 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = Age), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = zlimD, na.value = '#00008F')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 3.5, hjust = 0, vjust = 1, label = 'atop(bold("d) Age at recruitment (days)"))', parse = TRUE)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.89, 0.75),
        legend.direction  = 'vertical',
        legend.key.height = unit(15, 'pt'),
        legend.key.width  = unit(05, 'pt'),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

#=============================================================================#
# stat_summary_hex: mean N_constant by pixel
#=============================================================================#
p5 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_constant), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlimC, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 3.5, hjust = 0, vjust = 1, label = 'atop(bold("c) Super-Individual\'s Worth"))', parse = TRUE)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.89, 0.75),
        legend.direction  = 'vertical',
        legend.key.height = unit(15, 'pt'),
        legend.key.width  = unit(05, 'pt'),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p6 <- ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = N_constant), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlimF, na.value = '#800000')+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 3.5, hjust = 0, vjust = 1, label = 'atop(bold("f) Super-Individual\'s Worth"))', parse = TRUE)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
        legend.text  = element_text(size=7),
        legend.title = element_text(size=7, face= 'bold'),
        legend.position   = c(0.89, 0.75),
        legend.direction  = 'vertical',
        legend.key.height = unit(15, 'pt'),
        legend.key.width  = unit(05, 'pt'),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

# #=============================================================================#
# # stat_summary_hex: mean N_length by pixel
# #=============================================================================#
# p7 <- ggplot(data = df)+
#   stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_length), fun = sum, binwidth = c(bin_lon, bin_lat))+
#   scale_fill_gradientn(colours = tim.colors(64), limits = zlim4, na.value = '#800000')+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)+
#   annotate(geom='text', x = -95, y = -23.7, color = 'black', size = 3.5, hjust = 0, vjust = 1, label = 'Length-Dependent\nMortality')+
#   theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
#         axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
#         axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
#         axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
#         legend.text  = element_text(size=7),
#         legend.title = element_text(size=7, face= 'bold'),
#         legend.position   = c(0.91, 0.65),
#         legend.direction  = 'vertical',
#         legend.key.height = unit(13, 'pt'),
#         legend.key.width  = unit(05, 'pt'),
#         legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))
# 
# p8 <- ggplot(data = df)+
#   stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = N_length), fun = sum, binwidth = c(bin_lon, bin_lat))+
#   scale_fill_gradientn(colours = tim.colors(64), limits = zlim4, na.value = '#800000')+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)+
#   annotate(geom='text', x = -95, y = -23.7, color = 'black', size = 3.5, hjust = 0, vjust = 1, label = 'Length-Dependent\nMortality')+
#   theme(axis.text.x  = element_text(face='bold', color='black', size=7, angle=0),
#         axis.text.y  = element_text(face='bold', color='black', size=7, angle=0),
#         axis.title.x = element_text(face='bold', color='black', size=7, angle=0),
#         axis.title.y = element_text(face='bold', color='black', size=7, angle=90),
#         legend.text  = element_text(size=7),
#         legend.title = element_text(size=7, face= 'bold'),
#         legend.position   = c(0.91, 0.65),
#         legend.direction  = 'vertical',
#         legend.key.height = unit(13, 'pt'),
#         legend.key.width  = unit(05, 'pt'),
#         legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

# [2x3 draw] a,b,c,d,e,f 
png(filename = ggname, width = 950, height = 650, res = 120)
grid.arrange(p3, p1, p5, p4, p2, p6, nrow = 2)

# png(filename = ggname, width = 950, height = 350, res = 120)
# grid.arrange(p3, p1, p5, nrow = 1)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#