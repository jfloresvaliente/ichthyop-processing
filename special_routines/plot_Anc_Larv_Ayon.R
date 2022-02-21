#=============================================================================#
# Name   : plot_Anc_Larv_Ayon
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

dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/'
xlim     <- c(-85, -70) # Londitude
ylim     <- c(-20, 0)   # Latitude
bin_lon  <- .25
bin_lat  <- .25

zlim1 <- c(0, 350)
zlim2 <- c(0, 100)

# zlim1 <- c(0, 7000)
# zlim2 <- c(0, 4000)

# Plot's titles
xposlab <- -85
yposlab <- -18.7

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
df1 <- read.table(file = paste0(dirpath, 'Climatology_Anc_egg.csv'), header = T, sep = ';')
df1$lon <- df1$lon - 360

df2 <- read.table(file = paste0(dirpath, 'Climatology_Anc_lar.csv'), header = T, sep = ';')
df2$lon <- df2$lon - 360

#=============================================================================#
# stat_summary_hex: mean #egg & # larvae
#=============================================================================#
p1 <- ggplot(data = df1)+
  stat_summary_hex(data = df1, mapping = aes(x = lon, y = lat, z = Anc_egg), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim1, na.value = '#800000')+
  labs(x = 'Longitude', y = 'Latitude', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 4.5, hjust = 0, vjust = 1, label = 'atop(bold("g)"))', parse = TRUE)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=9, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=9, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=11, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=11, angle=90),
        legend.text  = element_text(size=8, face = 'bold'),
        legend.title = element_text(size=8, face = 'bold'),
        legend.position   = c(0.89, 0.75),
        legend.direction  = 'vertical',
        legend.key.height = unit(15, 'pt'),
        legend.key.width  = unit(05, 'pt'),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

p2 <- ggplot(data = df2)+
  stat_summary_hex(data = df2, mapping = aes(x = lon, y = lat, z = Anc_lar), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim2, na.value = '#800000')+
  labs(x = 'Longitude', y = 'Latitude', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  # annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 4.5, hjust = 0, vjust = 1, label = 'atop(bold("c)"))', parse = TRUE)+
  theme(axis.text.x  = element_text(face='bold', color='black', size=9, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=9, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=11, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=11, angle=90),
        legend.text  = element_text(size=8, face = 'bold'),
        legend.title = element_text(size=8, face = 'bold'),
        legend.position   = c(0.89, 0.75),
        legend.direction  = 'vertical',
        legend.key.height = unit(15, 'pt'),
        legend.key.width  = unit(05, 'pt'),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))

#=============================================================================#
# PLOTS
#=============================================================================#

# PNG plot
png(filename = paste0(dirpath, 'Anch_egg.png'), width = 700, height = 750, res = 120)
grid.arrange(p1, nrow = 1)
dev.off()

png(filename = paste0(dirpath, 'Anch_lar.png'), width = 700, height = 750, res = 120)
grid.arrange(p2, nrow = 1)
dev.off()

# TIFF plot
tiff(filename = paste0(dirpath, 'Anch_egg.tiff'), width = 700, height = 750, res = 120)
grid.arrange(p1, nrow = 1)
dev.off()

tiff(filename = paste0(dirpath, 'Anch_lar.tiff'), width = 700, height = 750, res = 120)
grid.arrange(p2, nrow = 1)
dev.off()

#SVG plot
svg(filename = paste0(dirpath, 'Anch_egg.svg'), width = 7.00, height = 7.50)
grid.arrange(p1, nrow = 1)
dev.off()

svg(filename = paste0(dirpath, 'Anch_lar.svg'), width = 7.00, height = 7.50)
grid.arrange(p2, nrow = 1)
dev.off()

#=============================================================================#
# END OF PROGRAM
#=============================================================================#