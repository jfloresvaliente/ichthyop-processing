library(ggplot2)
library(fields)

# Get data from the bibliography
# Castro - Hernandes 2000
dirpath  <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/bib_data/'
csv_file <- paste0(dirpath, 'CastroHernandesY1995.csv')
lab1     <- read.table(csv_file, header = T, sep = ';')
colnames(lab1) <- c('t', 'sl')
lab1$temp  <- 11
lab1$autor <- 'Castro - Hernandes 2000'
lab1$sl <- lab1$sl/ 10 # de mm a cm
lab1$temp <- as.factor(lab1$temp)
# plot(lab1$t, lab1$sl)

# Get laboratory data
# Aguirre 2000
dirpath  <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/engraulis_data/'
csv_file <- paste0(dirpath, 'Crecimiento_E.ringens_IMAPRE.csv')
lab2     <- read.table(csv_file, header = T, sep = ',')
lab2     <- lab2[, c(1,2,4)]
colnames(lab2) <- c('t', 'sl', 'temp')
lab2$autor <- 'Aguirre 2000'
lab2$sl  <- lab2$sl/10000 # de micras a cm
lab2$temp <- as.factor(lab2$temp)
# plot(lab2$t, lab2$sl)

lab <- rbind(lab1, lab2)

# Get DEB_out data
dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEB_out_s/'
txt_files <- list.files(path = dirpath, pattern = 'DEB_out', full.names = T)

functional_response <- seq(0.5, 1, 0.1)
temperature         <- c(11,13,15,18,19)
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
dat <- subset(dat, dat$t <= 35)

# PLOTS

ggname <- paste0(dirpath, 'DEB_outVSCastroHernandez2000ArturoLab.png')
ggplot(data = dat)+
  geom_line(mapping = aes(x = t, y = L_w, colour = temp), size = 1, linetype = 'solid')+
  # scale_color_manual(values = cols)+
  scale_color_manual(labels = c('11ºC Castro & Hernandez 2000', '14ºC Castro & Hernandez 2000', '15ºC Aguirre 2020', '18ºC Aguirre 2020', '19ºC Aguirre 2020'), values = cols) +
  facet_wrap(facets = ~f, ncol = 3, nrow = 2)+
  labs(x = 'Time in Days [d]', y = 'Standard Length [cm]', color = 'T [ºC]')+
  geom_point(data = lab1, mapping = aes(x = t, y = sl, colour = temp))+
  geom_point(data = lab2, mapping = aes(x = t, y = sl, colour = temp))+
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(0.1, 0.9),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)
