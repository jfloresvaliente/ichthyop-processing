library(ggplot2)

dirpath <- 'E:/ICHTHYOP/10kmparent/DEB/out/E_ringens/results/'
days <- 60 # Paso de tiempo

for(i in 1:12){

  Rfile <- paste0(dirpath, 'trajectoriesM', i, '.Rdata')
  print(Rfile)
  load(Rfile)
  
  dat <- trajectories; rm(trajectories)
  dat <- subset(dat, dat$Timer <= (days+1))
  
  index <- subset(dat, dat$Timer == (days+1) & dat$IfRecruited == 1)$Drifter
  dat <- subset(dat, dat$Drifter %in% index)
  dat$Timer <- dat$Timer-1
  
  p <-  ggplot(data = dat)+
    geom_smooth(data = dat, mapping = aes(x = Timer, y = length, colour = ReleaseBathy), method = 'lm')+
    labs(x = 'Age in Days [d]', y = 'Length [mm]')+
    coord_fixed(xlim = c(0,60), ylim = c(0,45))+
    theme(axis.text.x  = element_text(face='bold', color='black', size=15, angle=0),
          axis.text.y  = element_text(face='bold', color='black', size=15, angle=0),
          axis.title.x = element_text(face='bold', color='black', size=15, angle=0),
          axis.title.y = element_text(face='bold', color='black', size=15, angle=90),
          plot.title   = element_text(face='bold', color='black', size=15, angle=0),
          legend.text  = element_text(face='bold', color='black', size=15),
          legend.title = element_text(face='bold', color='black', size=15),
          legend.position   = c(0.15, 0.8),
          legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'))
  pngname <- paste0('C:/Users/jflores/Desktop/DEB_ReleaseBathy_M', i,'.png')
  ggsave(filename = pngname, plot = last_plot(), width = 8, height = 8)
}

