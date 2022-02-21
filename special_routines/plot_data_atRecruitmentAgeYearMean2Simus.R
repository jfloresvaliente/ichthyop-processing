#=============================================================================#
# Name   : plot_data_atRecruitmentAgeYearMean2Simus
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

df1 <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu1/results/data_atRecruitmentAge.Rdata'
df2 <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu7/results/data_atRecruitmentAge.Rdata'

xlim     <- c(-85, -70) # Londitude
ylim     <- c(-20, 0)   # Latitude
bin_lon  <- .1
bin_lat  <- .1
figname  <- paste0('C:/Users/jflores/Desktop/grid_arrange', bin_lat)

#========== Zlim ==========#
zlimA <- c(20,90) # Age at recruitment
zlimB <- c(0,150) # No mortality
zlimC <- c(0,2) # Constant mortality

zlimD <- zlimA    # Age at recruitment
zlimE <- c(0,150) # No mortality
zlimF <- c(0,1) # Constant mortality

# Plot's titles
xposlab <- -85
yposlab <- -18.7
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
load(df1); df1 <- df; df1 <- subset(df1, df1$IfRecruited == 1)
load(df2); df2 <- df; df2 <- subset(df2, df2$IfRecruited == 1)
rm(df)

#=============================================================================#
# stat_summary_hex: mean age recruited by pixel
#=============================================================================#
p1 <- ggplot(data = df1)+
  stat_summary_hex(data = df1, mapping = aes(x = Lon_ini, y = Lat_ini, z = Age), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = zlimA, na.value = '#800000')+
  labs(x = '', y = 'Latitude', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 4.5, hjust = 0, vjust = 1, label = 'atop(bold("a)"))', parse = TRUE)+
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

p4 <- ggplot(data = df2)+
  stat_summary_hex(data = df2, mapping = aes(x = Lon_ini, y = Lat_ini, z = Age), fun = mean, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = zlimD, na.value = '#800000')+
  labs(x = 'Longitude', y = 'Latitude', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 4.5, hjust = 0, vjust = 1, label = 'atop(bold("d)"))', parse = TRUE)+
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
# stat_bin_hex: count number of particles by pixel
#=============================================================================#
p2 <- ggplot(data = df1)+
  stat_bin_hex(data = df1, mapping = aes(x = Lon_ini, y = Lat_ini), binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlimB, na.value = '#800000')+
  labs(x = '', y = '', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 4.5, hjust = 0, vjust = 1, label = 'atop(bold("b)"))', parse = TRUE)+
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

p5 <- ggplot(data = df2)+
  stat_bin_hex(data = df2, mapping = aes(x = Lon_ini, y = Lat_ini), binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlimE, na.value = '#800000')+
  labs(x = 'Longitude', y = '', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 4.5, hjust = 0, vjust = 1, label = 'atop(bold("e)"))', parse = TRUE)+
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
# stat_summary_hex: mean N_constant by pixel
#=============================================================================#
p3 <- ggplot(data = df1)+
  stat_summary_hex(data = df1, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_constant), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlimC, na.value = '#800000')+
  labs(x = '', y = '', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 4.5, hjust = 0, vjust = 1, label = 'atop(bold("c)"))', parse = TRUE)+
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

p6 <- ggplot(data = df2)+
  stat_summary_hex(data = df2, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_constant), fun = sum, binwidth = c(bin_lon, bin_lat))+
  scale_fill_gradientn(colours = tim.colors(64), limits = zlimF, na.value = '#800000')+
  labs(x = 'Longitude', y = '', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)+
  annotate(geom='text', x = xposlab, y = yposlab, color = 'black', size = 4.5, hjust = 0, vjust = 1, label = 'atop(bold("f)"))', parse = TRUE)+
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
png(filename = paste0(figname,'.png'), width = 950, height = 750, res = 120)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
dev.off()

# TIFF plot
png(filename = paste0(figname,'.tiff'), width = 950, height = 750, res = 120)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
dev.off()

#SVG plot
svg(filename = paste0(figname,'.svg'), width = 9.50, height = 7.50)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#