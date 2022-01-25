#=============================================================================#
# Name   : plot_data_atRecruitmentAge
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(ggplot2)
library(fields)
library(hexbin)

dirpath  <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC1/out25C/results/'
xlim     <- c(-85, -70) # Londitude
ylim     <- c(-20, 0)   # Latitude

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

load(file = paste0(dirpath, 'data_atRecruitmentAge.Rdata'))

#=============================================================================#
# stat_bin_hex: count number of particles by pixel (0.25º)
#=============================================================================#
ggname <- paste0(dirpath, 'BinHex_Count_inipos.png')
ggplot(data = df)+
  stat_bin_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini), binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,150))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Count') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

ggname <- paste0(dirpath, 'BinHex_Count_endpos.png')
ggplot(data = df)+
  stat_bin_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end), binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,150))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Count') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

#=============================================================================#
# stat_summary_hex: mean age recruited by pixel (0.25º)
#=============================================================================#
ggname <- paste0(dirpath, 'BinHex_MeanAgeRecruited_inipos.png')
ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = c(15,50))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

ggname <- paste0(dirpath, 'BinHex_MeanAgeRecruited_endpos.png')
ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = rev(tim.colors(64)), limits = c(15,50))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

#=============================================================================#
# stat_summary_hex: mean N_length by pixel (0.25º)
#=============================================================================#
ggname <- paste0(dirpath, 'BinHex_MortalityLength_inipos.png')
ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_length), fun = sum, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,5))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

ggname <- paste0(dirpath, 'BinHex_MortalityLength_endpos.png')
ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = N_length), fun = sum, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,5))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

#=============================================================================#
# stat_summary_hex: mean N_constant by pixel (0.25º)
#=============================================================================#
ggname <- paste0(dirpath, 'BinHex_MortalityConstant_inipos.png')
ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_ini, y = Lat_ini, z = N_constant), fun = sum, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,10))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

ggname <- paste0(dirpath, 'BinHex_MortalityConstant_endpos.png')
ggplot(data = df)+
  stat_summary_hex(data = df, mapping = aes(x = Lon_end, y = Lat_end, z = N_constant), fun = sum, binwidth = c(.25,.25))+
  scale_fill_gradientn(colours = tim.colors(64), limits = c(0,10))+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# # Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
# ggname <- paste0(dirpath, 'BinHex_MeanAgeRecruitedReleaseDepth_inipos.png')
# ggplot(data = df)+
#   stat_summary_hex(data = df, mapping = aes(x = Lon, y = Lat, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
#   scale_fill_gradientn(colours = rev(tim.colors(64)))+
#   facet_wrap(facets = ~ ReleaseDepth, ncol = 3, nrow = 1)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)
# 
# # Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
# ggname <- paste0(dirpath, 'BinHex_MeanAgeRecruitedReleaseBathy_inipos.png')
# ggplot(data = df)+
#   stat_summary_hex(data = df, mapping = aes(x = Lon, y = Lat, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
#   scale_fill_gradientn(colours = rev(tim.colors(64)))+
#   facet_wrap(facets = ~ ReleaseBathy, ncol = 3, nrow = 1)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

 

#=============================================================================#
# PLOTS FOR FINAL POSITION
#=============================================================================#






# # Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
# ggname <- paste0(dirpath, 'BinHex_MeanAgeRecruitedReleaseDepth_endpos.png')
# ggplot(data = df)+
#   stat_summary_hex(data = df, mapping = aes(x = Lon, y = Lat, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
#   scale_fill_gradientn(colours = rev(tim.colors(64)))+
#   facet_wrap(facets = ~ ReleaseDepth, ncol = 3, nrow = 1)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)
# 
# # Count plot: bin_hex; sumarise of number of particles by pixel of 0.25º
# ggname <- paste0(dirpath, 'BinHex_MeanAgeRecruitedReleaseBathy_endpos.png')
# ggplot(data = df)+
#   stat_summary_hex(data = df, mapping = aes(x = Lon, y = Lat, z = AgeRecruited), fun = mean, binwidth = c(.25,.25))+
#   scale_fill_gradientn(colours = rev(tim.colors(64)))+
#   facet_wrap(facets = ~ ReleaseBathy, ncol = 3, nrow = 1)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Recruited \nAge') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# ggname <- paste0(dirpath, 'BinHex_MortalityConstant_endpos.png')
# ggplot(data = df)+
#   stat_summary_hex(data = df, mapping = aes(x = Lon, y = Lat, z = N_constant), fun = sum, binwidth = c(.25,.25))+
#   scale_fill_gradientn(colours = tim.colors(64))+
#   facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
#   labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
#   borders(fill='grey',colour='grey') +
#   coord_fixed(xlim = xlim, ylim = ylim)
# ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

#=============================================================================#
# END OF PROGRAM
#=============================================================================#