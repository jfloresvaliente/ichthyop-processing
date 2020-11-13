#=============================================================================#
# Name   : plot_mapRecruited
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(ggplot2)
library(fields)
library(hexbin)

dirpath  <- 'E:/ICHTHYOP/10kmparent/DEB/out/results/'
xlim     <- c(-85, -70) # Londitude
ylim     <- c(-20, 0)   # Latitude

load(file = paste0(dirpath, 'recruited_subset_inipos.Rdata'))
load(file = paste0(dirpath, 'recruited_subset_endpos.Rdata'))

#========== PLOTS FOR INITIAL POSITION ==========#
# Count plot: bin_hex; number of particles by pixel of 0.25º
ggname <- paste0(dirpath, 'BinHex_inipos.png')
ggplot(data = df_ini)+
  stat_bin_hex(data = df_ini, mapping = aes(x = Lon, y = Lat), binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,130))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Count') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
ggname <- paste0(dirpath, 'BinHexSummariseAgeRecruited_inipos.png')
ggplot(data = df_ini)+
  stat_summary_hex(data = df_ini, mapping = aes(x = Lon, y = Lat, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(15,60))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
ggname <- paste0(dirpath, 'BinHexSummariseAgeRecruitedReleaseDepth_inipos.png')
ggplot(data = df_ini)+
  stat_summary_hex(data = df_ini, mapping = aes(x = Lon, y = Lat, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = c(15,60))+
  facet_wrap(facets = ~ ReleaseDepth, ncol = 3, nrow = 1)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
ggname <- paste0(dirpath, 'BinHexSummariseAgeRecruitedReleaseBathy_inipos.png')
ggplot(data = df_ini)+
  stat_summary_hex(data = df_ini, mapping = aes(x = Lon, y = Lat, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = c(15,60))+
  facet_wrap(facets = ~ ReleaseBathy, ncol = 3, nrow = 1)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# # Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
# ggname <- paste0(dirpath, 'BinHexSummariseLength_inipos.png')
# ggplot(data = df_ini)+
#   stat_summary_hex(data = df_ini, mapping = aes(x = Lon, y = Lat, z = length), fun = mean, binwidth = c(.25,.25))+
#   scale_fill_gradientn(colours = tim.colors(64))+
#   facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'length') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# # Density 2D plot: 'ndensity'
# ggname <- paste0(dirpath, 'RecruitedDensity2D_inipos.png')
# ggplot(data = df_ini)+
#   geom_density_2d_filled(data = df_ini, mapping = aes(x = Lon, y = Lat), contour_var = 'ndensity')+
#   facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Density') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)
# 
# # Density 2D plot: 'count'
# ggname <- paste0(dirpath, 'RecruitedCount2D_inipos.png')
# ggplot(data = df_ini)+
#   geom_density_2d_filled(data = df_ini, mapping = aes(x = Lon, y = Lat), contour_var = 'count')+
#   facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Count') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)
# 
# # Scatter plot
# ggname <- paste0(dirpath, 'RecruitedAgeScatter_inipos.png')
# ggplot(data = df_ini)+
#   geom_point(data = df_ini, aes(x = Lon, y = Lat, colour = AgeRecruited), size = .05) +
#   scale_colour_gradientn(colours = tim.colors(n = 64, alpha = 1), expression('Recruited \nAge')) +
#   facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

#========== PLOTS FOR FINAL POSITION ==========#
# Count plot: bin_hex; number of particles by pixel of 0.25º
ggname <- paste0(dirpath, 'BinHex_endpos.png')
ggplot(data = df_end)+
  stat_bin_hex(data = df_end, mapping = aes(x = Lon, y = Lat), binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,300))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Count') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
ggname <- paste0(dirpath, 'BinHexSummariseAgeRecruited_endpos.png')
ggplot(data = df_end)+
  stat_summary_hex(data = df_end, mapping = aes(x = Lon, y = Lat, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(15,60))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
ggname <- paste0(dirpath, 'BinHexSummariseAgeRecruitedReleaseDepth_endpos.png')
ggplot(data = df_end)+
  stat_summary_hex(data = df_ini, mapping = aes(x = Lon, y = Lat, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = c(15,60))+
  facet_wrap(facets = ~ ReleaseDepth, ncol = 3, nrow = 1)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
ggname <- paste0(dirpath, 'BinHexSummariseAgeRecruitedReleaseBathy_endpos.png')
ggplot(data = df_end)+
  stat_summary_hex(data = df_ini, mapping = aes(x = Lon, y = Lat, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = c(15,60))+
  facet_wrap(facets = ~ ReleaseBathy, ncol = 3, nrow = 1)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# # Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
# ggname <- paste0(dirpath, 'BinHexSummariseLength_endpos.png')
# ggplot(data = df_end)+
#   stat_summary_hex(data = df_end, mapping = aes(x = Lon, y = Lat, z = length), fun = mean, binwidth = c(.25,.25))+
#   scale_fill_gradientn(colours = tim.colors(64))+
#   facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'length') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# # Density 2D plot: 'ndensity'
# ggname <- paste0(dirpath, 'RecruitedDensity2D_endpos.png')
# ggplot(data = df_end)+
#   geom_density_2d_filled(data = df_end, mapping = aes(x = Lon, y = Lat), contour_var = 'ndensity')+
#   facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Density') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)
# 
# # Density 2D plot: 'count'
# ggname <- paste0(dirpath, 'RecruitedCount2D_endpos.png')
# ggplot(data = df_end)+
#   geom_density_2d_filled(data = df_end, mapping = aes(x = Lon, y = Lat), contour_var = 'count')+
#   facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Count') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)
# 
# # Scatter plot of recruited age
# ggname <- paste0(dirpath, 'RecruitedAgeScatter_endpos.png')
# ggplot(data = df_end)+
#   geom_point(data = df_end, aes(x = Lon, y = Lat, colour = AgeRecruited), size = .025) +
#   scale_colour_gradientn(colours = tim.colors(n = 64, alpha = 1), expression('Recruited \nAge')) +
#   facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# geom_density2d_filled(breaks = vector  que define los niveles,
#                       bins   = integer que define el numero de niveles)
# scale_fill_manual(values = tim.colors(n = debe ser igual a bins o breaks)) 
#=============================================================================#
# END OF PROGRAM
#=============================================================================#