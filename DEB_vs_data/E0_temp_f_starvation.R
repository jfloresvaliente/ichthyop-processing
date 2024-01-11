library(ggplot2)
library(gridExtra)

dirpath <- c('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEB_E0out/',
             'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEB_E0out/')

sp <- c('E. encrasicolus', 'E. ringens')

if(file.exists('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/E0_temp_starvation.Rdata')){
  load('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/E0_temp_starvation.Rdata')
}else{
  m <- NULL
  for(i in 1:length(dirpath)){
    txt_files <- list.files(path = dirpath[i], pattern = '.txt', full.names = T)
    
    df <- NULL
    for(j in 1:length(txt_files)){
      print(txt_files[j])
      dat <- read.table(txt_files[j], header = T, sep = ',')
      ind <- which(dat$E <= 0)[1] # Elegir el momento antes de que E_0 sea negativo = muerte
      dat <- c(dat$t[ind], dat$E0[ind], dat$temp[ind])
      df  <- rbind(df, dat)
    }
    df <- as.data.frame(df)
    colnames(df) <- c('t', 'E0', 'temp')
    df$sp <- sp[i]
    
    m <- rbind(m,df)
  }
  rownames(m) <- NULL
  
  RData <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/E0_temp_starvation.Rdata'
  save(m, file = RData)
}

#----------  ----------  ----------  ----------#
m <- na.omit(object = m)
m <- subset(m, m$temp %in% c(16,20,24))

m$temp <- paste(m$temp, 'ºC')

p1 <- ggplot(data = m, mapping = aes(x = E0, y = t, colour = sp))+
  # geom_line()+
  geom_point()+
  facet_wrap(~temp)+
  labs(x = 'Initial Reserve (E0, J)', y = 'Starvation Age (d)') +
  # coord_fixed(xlim = xlim, ylim = ylim, ratio = 2/2) +
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=15, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=15, angle=90),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=0),
        legend.position   = c(0.9, 0.87),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap)

png(filename = paste0('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/E0_temp_starvation.png'), width = 1050, height = 550, res = 120)
grid.arrange(p1, nrow = 1)
dev.off()