#=============================================================================#
# Name   : 
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

# Get DEB_out data
dirpath <- c('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEBoutV2/cTcase2/',
             'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEBoutV2/cTcase2/')

L_b  <- 0.52   # J, Maturity threshold at birth % ouverture de la bouche
L_j  <- 1.38   # J, Maturity threshold at metamorphosis
L_p  <- 9.52   # J, Maturity threshold at puberty

functional_response <- seq(1)
temperature         <- 14:24

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dat <- NULL

for(k in 1:length(dirpath)){
  for(i in 1:length(functional_response)){
    for(j in 1:length(temperature)){
      
      txt_file <- paste0(dirpath[k], 'DEB_outT', temperature[j],'f',functional_response[i],'.txt')
      df <- read.table(file = txt_file, header = T, sep = ',')
      
      # Identificamos de que especie se trata:
      if(grepl(pattern = 'ringens', x = txt_file)) df$sp <- 'E. ringens' else df$sp <- 'E. encrasicolus'
      
      # If it is necessary to calculate the physical length
      if(sum(grepl(pattern = 'Lw', x = names(df))) == 0){
        df$Lw <- df$V^(1/3)/df$delta
      }
      
      print(txt_file)

      # birth index
      i_b <- which(df$Lw >= L_b)[1]
      t_b <- df$t[i_b]
      Lb <- df$Lw[i_b]
      
      # metamorphosis index
      i_j <- which(df$Lw >= L_j)[1]
      t_j <- df$t[i_j]
      Lj <- df$Lw[i_j]
      
      # puberty index
      i_p <- which(df$Lw >= L_p)[1]
      t_p <- df$t[i_p]
      Lp <- df$Lw[i_p]
      
      val_in <- c(t_b, Lb, t_j, Lj,t_p, Lp, temperature[j], functional_response[i], df$sp[1])
      dat <- rbind(dat, val_in)
    }
  }
  
}

colnames(dat) <- c('age_birth', 'length_birth', 'age_metamorphosis', 'length_metamorphosis', 'age_puberty', 'length_puberty', 'temp', 'f','sp')
dat <- as.data.frame(dat)
row.names(dat) <- NULL

dat$age_birth         <- as.numeric(dat$age_birth)
dat$age_metamorphosis <- as.numeric(dat$age_metamorphosis)
dat$age_puberty       <- as.numeric(dat$age_puberty)
dat$sp                <- as.factor(dat$sp)

p1 <- ggplot(data = dat, mapping = aes(x = temp, y = age_birth, colour = sp))+
  geom_point()+
  labs(x = 'Temperature (ºC)', y = 'Age at birth (d)', linetype = '', colour = 'sp')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=13, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=13, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=15, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=15, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        plot.margin  = unit(c(.5,4,.5,.5),'cm'),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(.8, .75),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face = 'bold', color = 'black', size = 12)) # Para cambiar el tamaño del título en facet_wrap
png(filename = 'C:/Users/jflores/Desktop/age_birth2SP.png', width = 1050, height = 850, res = 120)
grid.arrange(p1, nrow = 1)
dev.off()

p2 <- ggplot(data = dat, mapping = aes(x = temp, y = age_metamorphosis, colour = sp))+
  geom_point()+
  labs(x = 'Temperature (ºC)', y = 'Age at metamorphosis (d)', linetype = '', colour = 'sp')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=13, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=13, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=15, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=15, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        plot.margin  = unit(c(.5,4,.5,.5),'cm'),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(.8, .75),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face = 'bold', color = 'black', size = 12)) # Para cambiar el tamaño del título en facet_wrap
png(filename = 'C:/Users/jflores/Desktop/age_metamorphosis2SP.png', width = 1050, height = 850, res = 120)
grid.arrange(p2, nrow = 1)
dev.off()

p3 <- ggplot(data = dat, mapping = aes(x = temp, y = age_puberty, colour = sp))+
  geom_point()+
  labs(x = 'Temperature (ºC)', y = 'Age at puberty (d)', linetype = '', colour = 'sp')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=13, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=13, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=15, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=15, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        plot.margin  = unit(c(.5,4,.5,.5),'cm'),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(.8, .75),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face = 'bold', color = 'black', size = 12)) # Para cambiar el tamaño del título en facet_wrap
png(filename = 'C:/Users/jflores/Desktop/age_puberty2SP.png', width = 1050, height = 850, res = 120)
grid.arrange(p3, nrow = 1)
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#