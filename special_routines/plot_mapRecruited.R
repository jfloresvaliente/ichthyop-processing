#=============================================================================#
# Name   : plot_mapAgeRecruited
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(ggplot2)
library(fields)

dirpath <- 'E:/ICHTHYOP/10kmparent/DEB/out/results/'
days <- 60

#========== Do not change anything after here ==========#
df_ini <- NULL # Data frame for initial positions of recruited particles
df_end <- NULL # Data frame for final positions of recruited particles

for(j in 1:12){
  Rdata <- paste0(dirpath,'trajectoriesM',j,'.Rdata')
  load(Rdata)
  print(Rdata)
  dat <- trajectories; rm(trajectories)
  
  index <- subset(dat, dat$Timer == (days+1) & dat$IfRecruited == 1)$Drifter
  
  recruited <- subset(dat, dat$Drifter %in% index)
  recruited <- matrix(data = recruited$IfRecruited, ncol = (days+1), nrow = dim(recruited)[1]/(days+1), byrow = T)
  recruited_day    <- apply(recruited, 1, which.max)
  
  for(u in seq_along(recruited_day)){
    inicol <- recruited_day[u]
  
    if(inicol == days+1) next()
    recruited[u, (inicol+1) : dim(recruited)[2]] <- NA
   }
  
  recruited <- as.vector(t(recruited))
  
  dat <- subset(dat, dat$Drifter %in% index)
  dat <- cbind(dat, recruited)

  dat <- subset(dat, dat$recruited == 1)
  dat$AgeRecruited <- recruited_day-1
  dat$Month <- j
  
  for(i in 1:9){
    zone_name <- paste0('zone', i)
    a <- grep(pattern = zone_name, x = dat$Zone_name)
    dat$Zone_name[a] <- zone_name
  }
  
  # Get an aleatory sample of 1000 particles
  # set.seed(10^2)
  # dat <- dat[sample(nrow(dat), 1000), ]
  
  names_col <- c('Drifter', 'Lon', 'Lat', 'Depth', 'Zone_name', 'ReleaseDepth', 'ReleaseBathy', 'AgeRecruited', 'Month')
  names_ind <- NULL
  for(n in seq_along(names_col)){
    names_ind <- c(names_ind, which(names(dat) == names_col[n]))
  }
  dat <- dat[, names_ind]
  
  df_end <- rbind(df_end, dat)
  
  recruited_drifters <- dat$Drifter
  load(Rdata)
  dat2 <- trajectories; rm(trajectories)
  dat2 <- subset(dat2, dat2$Drifter %in% recruited_drifters & dat2$Timer == 1)
  
  names_col <- c('Drifter', 'Lon', 'Lat', 'Depth')
  names_ind <- NULL
  for(n in seq_along(names_col)){
    names_ind <- c(names_ind, which(names(dat2) == names_col[n]))
  }
  
  dat2 <- cbind(dat2[,names_ind], dat$Zone_name, dat$ReleaseDepth, dat$ReleaseBathy, dat$AgeRecruited, dat$Month)
  colnames(dat2) <- names(dat)
  df_ini <- rbind(df_ini, dat2)
  
  rm(dat, dat2)
}

df_end <- df_end[,-c(1)]; df_end$Month <- as.factor(df_end$Month)
df_ini <- df_ini[,-c(1)]; df_ini$Month <- as.factor(df_ini$Month)

xlim = c(-85, -70) # Londitude
ylim = c(-20, 0)   # Latitude

#========== PLOTS FOR INITIAL POSITION ==========#
# Density 2D plot: 'ndensity'
ggname <- paste0(dirpath, 'RecruitedDensity2D_inipos.png')
ggplot(data = df_ini)+
  geom_density_2d_filled(data = df_ini, mapping = aes(x = Lon, y = Lat), contour_var = 'ndensity')+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Density') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# Density 2D plot: 'count'
ggname <- paste0(dirpath, 'RecruitedCount2D_inipos.png')
ggplot(data = df_ini)+
  geom_density_2d_filled(data = df_ini, mapping = aes(x = Lon, y = Lat), contour_var = 'count')+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Count') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# Scatter plot
ggname <- paste0(dirpath, 'RecruitedAgeScatter_inipos.png')
ggplot(data = df_ini)+
  geom_point(data = df_ini, aes(x = Lon, y = Lat, colour = AgeRecruited), size = .05) +
  scale_colour_gradientn(colours = tim.colors(n = 64, alpha = 1), expression('Recruited \nAge')) +
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

#========== PLOTS FOR FINAL POSITION ==========#
# Density 2D plot: 'ndensity'
ggname <- paste0(dirpath, 'RecruitedDensity2D_endpos.png')
ggplot(data = df_end)+
  geom_density_2d_filled(data = df_end, mapping = aes(x = Lon, y = Lat), contour_var = 'ndensity')+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Density') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# Density 2D plot: 'count'
ggname <- paste0(dirpath, 'RecruitedCount2D_endpos.png')
ggplot(data = df_end)+
  geom_density_2d_filled(data = df_end, mapping = aes(x = Lon, y = Lat), contour_var = 'count')+
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = 'Count') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# Scatter plot of recruited age
ggname <- paste0(dirpath, 'RecruitedAgeScatter_endpos.png')
ggplot(data = df_end)+
  geom_point(data = df_end, aes(x = Lon, y = Lat, colour = AgeRecruited), size = .025) +
  scale_colour_gradientn(colours = tim.colors(n = 64, alpha = 1), expression('Recruited \nAge')) +
  facet_wrap(facets = ~ Month, ncol = 4, nrow = 3)+
  labs(x = 'Longitude (W)', y = 'Latitude (S)') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(filename = ggname, plot = last_plot(), width = 10, height = 8)

# geom_density2d_filled(breaks = vector  que define los niveles,
#                       bins   = integer que define el numero de niveles)
# scale_fill_manual(values = tim.colors(n = debe ser igual a bins o breaks)) 
#=============================================================================#
# END OF PROGRAM
#=============================================================================#