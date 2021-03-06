library(ggplot2)
library(fields)

# Get data from the bibliography
dirpath  <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/bib_data/'
csv_file <- c('Leal2009ChileNorte.txt', 'Leal2009ChileSur.txt')

lab <- NULL
for(i in 1:length(csv_file)){
  lab <- rbind(lab, read.table(paste0(dirpath, csv_file[i])))
}
lab <- exp(lab)
colnames(lab) <- c('TotalLength', 'TotalWeight')

# Get DEB_out data
dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEB_out_t/'
txt_files <- list.files(path = dirpath, pattern = 'DEB_out', full.names = T)

functional_response <- seq(0.5, 1, 0.1)
temperature         <- c(12,14,16)
cols                <- tim.colors(n = length(temperature))

dat <- NULL
for(i in 1:length(functional_response)){
  for(j in 1:length(temperature)){
    
    txt_file <- paste0(dirpath, 'DEB_outT', temperature[j],'f',functional_response[i],'.txt')
    dat <- rbind(dat, read.table(file = txt_file, header = T, sep = ','))
    print(txt_file)
  }
}

dat$temp <- as.factor(dat$temp)
dat$f    <- paste('f =', as.factor(dat$f))
dat$Ww   <- dat$W_E + dat$W_V + dat$W_ER
# dat <- subset(dat, dat$t <= 1200)

# PLOTS

ggname <- paste0(dirpath, 'DEB_outVSLeal2009.png')
ggplot(data = dat)+
  geom_line(mapping = aes(x = L_w, y = Ww, colour = temp), size = 1, linetype = 'solid')+
  scale_color_manual(values = cols)+
  facet_wrap(facets = ~f, ncol = 3, nrow = 2)+
  labs(x = 'Total Length [cm]', y = 'Total Weight [d]', color = 'T [ºC]')+
  geom_point(data = lab, mapping = aes(x = TotalLength, y = TotalWeight))+
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(0.03, 0.9),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)
