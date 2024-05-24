#=============================================================================#
# Name   : DEB_out_stdabj_age_transitions
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
dirpath <- c('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEBoutV2/cTcase1/',
             'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEBoutV2/cTcase1/')

L_b  <- 0.52   # J, Maturity threshold at birth % ouverture de la bouche
L_j  <- 1.38   # J, Maturity threshold at metamorphosis
L_p  <- 9.52   # J, Maturity threshold at puberty

functional_response <- seq(1)
temperature         <- 14:21
cols <- c('red', 'blue')

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
      # if(grepl(pattern = 'ringens', x = txt_file)) df$sp <- 'E. ringens' else df$sp <- 'E. encrasicolus'
      if(grepl(pattern = 'ringens', x = txt_file)) df$sp <- 'DEBabj' else df$sp <- 'DEBstd'
      
      # If it is necessary to calculate the physical length
      if(sum(grepl(pattern = 'Lw', x = names(df))) == 0){
        df$Lw <- df$V^(1/3)/df$delta
      }
      
      print(txt_file)

      # birth index
      i_b <- which(df$Lw >= L_b)[1]
      t_b <- df$t[i_b]
      Lb <- df$Lw[i_b]
      birth <- cbind(t_b, Lb, temperature[j], functional_response[i], 'Age at birth (d)', df$sp[1])
      
      # metamorphosis index
      i_j <- which(df$Lw >= L_j)[1]
      t_j <- df$t[i_j]
      Lj <- df$Lw[i_j]
      metam <- cbind(t_j, Lj, temperature[j], functional_response[i], 'Age at metamorphosis (d)', df$sp[1])
      
      # puberty index
      i_p <- which(df$Lw >= L_p)[1]
      t_p <- df$t[i_p]
      Lp <- df$Lw[i_p]
      puber <- cbind(t_p, Lp, temperature[j], functional_response[i], 'Age at puberty (d)', df$sp[1])

      # val_in <- rbind(birth, metam, puber)
      val_in <- rbind(birth, metam)
      dat <- rbind(dat, val_in)
    }
  }
  
}

colnames(dat) <- c('age', 'length', 'temp', 'f', 'transition','sp')
dat <- as.data.frame(dat)
row.names(dat) <- NULL

dat$age        <- as.numeric(dat$age)
dat$length     <- as.numeric(dat$length)
dat$temp       <- as.numeric(dat$temp)
dat$f          <- as.factor(dat$f)
dat$transition <- as.factor(dat$transition)
dat$sp         <- as.factor(dat$sp)

dat_text <- data.frame(
  label      = c('a)', 'b)'),
  transition = c('Age at birth (d)', 'Age at metamorphosis (d)'),
  x = c(14.3, 14.3),
  y = c(8,46)
)

# dat_text <- data.frame(
#   label      = c('a)', 'b)', 'c)'),
#   transition = c('Age at birth (d)', 'Age at metamorphosis (d)', 'Age at puberty (d)'),
#   x = c(14.3, 14.3, 14.3),
#   y = c(8,46, 400)
# )

#------------- PLOTS -------------#

ggname <- paste0('C:/Users/jflores/Desktop/age_transitions2SP.png')
ggplot(data = dat)+
  geom_point(mapping = aes(x = temp, y = age, colour = sp), size = 3)+
  scale_color_manual(values = cols, 'Model Type')+
  labs(x = 'Temperature [ºC]', y = 'Time [d]')+
  facet_wrap(~transition, scales = 'free')+
  geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label),
    hjust   = 0,
    vjust   = 0,
    size = 8
  )+
  theme(axis.text.x  = element_text(face='bold', color='black', size=20, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=20, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=20, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=20, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        plot.margin  = unit(c(.5,4,.5,.5),'cm'),
        legend.text  = element_text(face='bold', color='black', size=17),
        legend.title = element_text(face='bold', color='black', size=17),
        legend.position   = c(0.38, .85),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face = 'bold', color = 'black', size = 20)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 16, height = 8)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#