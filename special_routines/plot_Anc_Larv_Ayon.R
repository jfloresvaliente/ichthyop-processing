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
outpath <- dirpath

xlim     <- c(-85, -70) # Londitude
ylim     <- c(-20, 0)   # Latitude
bin_lon  <- .25
bin_lat  <- .25

zlim1 <- c(0, 350)
zlim2 <- c(0, 100)

# zlim1 <- c(0, 7000)
# zlim2 <- c(0, 4000)

# Plot's titles
xposlab <- -82
yposlab <- -23.7

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
  # annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 4.5, hjust = 0, vjust = 1, label = 'atop(bold("e)"))', parse = TRUE)+
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

p2 <- ggplot(data = df2)+
  stat_summary_hex(data = df2, mapping = aes(x = lon, y = lat, z = Anc_lar), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlim2, na.value = '#800000')+
  labs(x = 'Longitude', y = 'Latitude', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  # annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 4.5, hjust = 0, vjust = 1, label = 'atop(bold("c)"))', parse = TRUE)+
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

#=============================================================================#
# PLOTS
#=============================================================================#

# # PNG plot
# png(filename = paste0(outpath, 'Anch_egg.png'), width = 750, height = 850, res = 120)
# grid.arrange(p1, nrow = 1)
# dev.off()
# 
# png(filename = paste0(outpath, 'Anch_lar.png'), width = 750, height = 850, res = 120)
# grid.arrange(p2, nrow = 1)
# dev.off()

# TIFF plot
tiff(filename = paste0(outpath, 'Anch_egg.tiff'), width = 750, height = 850, res = 120)
grid.arrange(p1, nrow = 1)
dev.off()

tiff(filename = paste0(outpath, 'Anch_lar.tiff'), width = 750, height = 850, res = 120)
grid.arrange(p2, nrow = 1)
dev.off()

# #SVG plot
# svg(filename = paste0(outpath, 'Anch_egg.svg'), width = 7.50, height = 8.50)
# grid.arrange(p1, nrow = 1)
# dev.off()
# 
# svg(filename = paste0(outpath, 'Anch_lar.svg'), width = 7.50, height = 8.50)
# grid.arrange(p2, nrow = 1)
# dev.off()

#=============================================================================#
# END OF PROGRAM
#=============================================================================#