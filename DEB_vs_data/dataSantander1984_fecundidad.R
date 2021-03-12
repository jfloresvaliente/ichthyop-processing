library(ggplot2)
library(fields)

# age <- 200 # Limite del eje X en dias
# 
# Get data from the bibliography
dirpath  <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/bib_data/'

lab <- read.table(file = paste0(dirpath, 'Santander et al 1984 tabla2.csv'), header = T, sep = ';')
lab$PesoGonada <- lab$PesoTotal - lab$PesoSinGonada
lab$GSI <- (lab$PesoGonada/lab$PesoTotal)*100

mod1 <- lm(formula = PesoTotal ~ PesoSinGonada, data = lab)
cof <- mod1$coefficients
mod1X <- 5:46
mod1Y <- cof[1] + cof[2] * mod1X

lab <- read.table(file = paste0(dirpath, 'Santander et al 1984 tabla3.csv'), header = T, sep = ';')
lab$PesoTotal <- cof[1] + cof[2] * lab$PesoSinGonada

# Get DEB_out data
dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/PickSpaw2/DEB_out_s/'

functional_response <- seq(0.5, 1, 0.1)
temperature         <- c(14,16,18,20)
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

# El periodo de desove es de aproximadamente 1.5 meses = 45 dias, el desove es cada 6.2 días 
dat <- dat[which(dat$F > 0),]
dat$batch <- round(dat$F/(45/6.2))
dat$batch <- round(dat$F/(45/10))

# PLOTS

ggname <- paste0(dirpath, 'Batch.png')
ggplot(data = dat)+
  # geom_point(data = dat, mapping = aes(x = Ww, y = batch, colour = temp), size = 2)+
  geom_point(data = lab, mapping = aes(x = PesoTotal, y = FecundidadParcial), size = .75)+
  geom_line (data = dat, mapping = aes(x = Ww, y = batch, colour = temp), size = 1, linetype = 'solid')+
  scale_color_manual(values = cols)+
  facet_wrap(facets = ~f, ncol = 3, nrow = 2)+
  labs(x = 'Total weight [g]', y = 'Partial Fecundity [#eggs/batch]', color = 'T [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(0.03, 0.9),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del titulo en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)
