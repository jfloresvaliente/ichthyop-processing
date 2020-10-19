library(ggplot2)
dirpath <- 'E:/ICHTHYOP/10kmparent/DEB/out/mediterraneo/results/'
days <- 60 # Paso de tiempo

release_depth <- NULL
release_bathy <- NULL
for(i in 1:12){
  
  Rfile <- paste0(dirpath, 'trajectoriesM', i, '.Rdata')
  print(Rfile)
  load(Rfile)
  
  dat <- trajectories; rm(trajectories)
  dat <- subset(dat, dat$Timer <= (days+1))
  
  index <- subset(dat, dat$Timer == (days+1) & dat$IfRecruited == 1)$Drifter
  recruited <- subset(dat, dat$Drifter %in% index)
  recruited$Timer <- recruited$Timer - 1
  
  depth <- tapply(X = recruited$length, INDEX = list(recruited$Timer, recruited$ReleaseDepth), FUN = mean)
  depth_vec <- as.vector(depth)
  depth <- as.data.frame(depth)
  varcol<- rep(names(depth), each = days+1)
  age <- rep(0:days, times = 3)
  depth <- data.frame(varcol, as.vector(depth_vec), age)
  colnames(depth) <- c('ReleaseDepth', 'Length', 'Age')
  release_depth <- rbind(release_depth, depth)
  
  bathy <- tapply(X = recruited$length, INDEX = list(recruited$Timer, recruited$ReleaseBathy), FUN = mean)
  bathy_vec <- as.vector(bathy)
  bathy <- as.data.frame(bathy)
  varcol<- rep(names(bathy), each = days+1)
  age <- rep(0:days, times = 3)
  bathy <- data.frame(varcol, as.vector(bathy_vec), age)
  colnames(bathy) <- c('ReleaseBathy', 'Length', 'Age')
  release_bathy <- rbind(release_bathy, bathy)
}

# Plot GGPLOT
p <- ggplot(data = release_depth)+
  # geom_point(data = release_depth, mapping = aes(x = Age, y = Length, colour = ReleaseDepth))+
  geom_smooth(data = release_depth, mapping = aes(x = Age, y = Length, colour = ReleaseDepth), method = 'loess')+
  # geom_line(data = release_depth, mapping = aes(x = Age, y = Length, colour = ReleaseDepth), size = 1.5, linetype = 'solid')+
  labs(x = 'Time in Days [d]', y = 'Length [mm]')+
  coord_fixed(xlim = c(0,60), ylim = c(0,60)) +
  theme(axis.text.x  = element_text(face='bold', color='black', size=15, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=15, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=15, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=15, angle=90),
        plot.title   = element_text(face='bold', color='black', size=15, angle=0),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.15, 0.9),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'))
ggsave(filename = paste0(dirpath, 'GrowthReleaseDepth.png'), plot = last_plot(), width = 8, height = 8)


# Plot2 GGPLOT
p <- ggplot(data = release_bathy)+
  # geom_point(data = release_bathy, mapping = aes(x = Age, y = Length, colour = ReleaseBathy))+
  geom_smooth(data = release_bathy, mapping = aes(x = Age, y = Length, colour = ReleaseBathy), method = 'loess')+
  # geom_line(data = release_bathy, mapping = aes(x = Age, y = Length, colour = ReleaseBathy), size = 1.5, linetype = 'solid')+
  labs(x = 'Time in Days [d]', y = 'Length [mm]')+
  coord_fixed(xlim = c(0,60), ylim = c(0,60)) +
  theme(axis.text.x  = element_text(face='bold', color='black', size=15, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=15, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=15, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=15, angle=90),
        plot.title   = element_text(face='bold', color='black', size=15, angle=0),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.15, 0.9),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'))
ggsave(filename = paste0(dirpath, 'GrowthReleaseBathy.png'), plot = last_plot(), width = 8, height = 8)
