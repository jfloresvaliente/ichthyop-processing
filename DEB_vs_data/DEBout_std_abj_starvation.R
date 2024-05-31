#=============================================================================#
# Name   : DEBout_std_abj_starvation
# Author : Jorge Flores-Valiente
# Date   :
# Version:
# Aim    : 
# URL    :
#=============================================================================#
library(ggplot2)
library(gridExtra)
library(stringr)
library(fields)

dirpath <- c('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEBoutV4/cTcase2/',
             'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEBoutV4/cTcase2/')

Lw <- c(0.5, 3)
cols <- c('red', 'blue')

dat <- NULL
for(k in 1:length(dirpath)){
  for(i in 1:length(Lw)){
    directory_out <- paste0(dirpath[k], 'Lw',Lw[i], '/')
    files_f0 <- list.files(path = directory_out, pattern = '.txt', full.names = T)
    
    for(j in 1:length(files_f0)){
      print(files_f0[j])
      df <- read.table(files_f0[j], header = T, sep = ',')
      df   <- df[-dim(df)[1],] # The last row containing an inf value is deleted.
      
      # Identificamos de que especie se trata:
      # if(grepl(pattern = 'ringens', x = files_f0[j])) df$sp <- 'E. ringens' else df$sp <- 'E. encrasicolus'
      if(grepl(pattern = 'ringens', x = files_f0[j])) df$sp <- 'DEBabj' else df$sp <- 'DEBstd'
      
      ini <- df$t[which(df$f == 0)[1]]
      fin <- df$t[which(df$starvation == 1)[1]]
      # # En caso de inanicion
      # ind <- which(df$starvation == 1)[1] # Elegir el momento antes de que E_0 sea negativo = muerte
      # if(is.na(ind)){
      #   df <- df[1,]
      #   
      #   # New data frame
      #   age_starvation <- NA
      #   Lw_ini         <- paste(Lw[i], 'cm')
      #   Lw_end         <- NA
      #   temp           <- df$temp
      #   sp             <- df$sp
      # }else{
      #   df <- df[(ind),]
      #   # New data frame
      #   age_starvation <- df$t
      #   Lw_ini         <- paste(Lw[i], 'cm')
      #   Lw_end         <- df$Lw
      #   temp           <- df$temp
      #   sp             <- df$sp
      # }
      
      df <- cbind(ini, fin, df$temp[1], df$sp[1], (fin-ini), paste(Lw[i], 'cm'))
      colnames(df) <- c('age_ini', 'age_fin', 'temp', 'sp', 'starvation','talla_ini')
      dat <- rbind(dat, df)
    }
  }
}

dat            <- as.data.frame(dat)
dat$age_ini    <- as.numeric(dat$age_ini)
dat$age_fin    <- as.numeric(dat$age_fin)
dat$temp       <- as.numeric(dat$temp)
dat$sp         <- as.factor(dat$sp)
dat$starvation <- as.numeric(dat$starvation)
dat$talla_ini  <- as.factor(dat$talla_ini)

dat$starvation[dat$starvation <= 0] <- NA
dat <- subset(dat, dat$temp >= 14)
# dat <- subset(dat, dat$temp >= 14 & dat$temp <= 24)

rownames(dat) <- NULL

#------------- PLOTS -------------#

dat_text <- data.frame(
  label     = paste0(letters[1:length(levels(factor(dat$sp)))], ')'),
  talla_ini = paste(levels(factor(dat$talla_ini)))
)

ggname <- paste0('C:/Users/jflores/Desktop/t_starvation_f0_V2.png')
ggplot(data = dat)+
  geom_point(data = dat, mapping = aes(x = temp, y = starvation, colour = sp), size = 2)+
  labs(x = 'Temperature [ºC]', y = 'Time to starvation [d]', linetype = 'Initial\nStandard Length\n(cm)', colour = 'sp')+
  scale_color_manual(values = cols, 'Model Type')+
  coord_fixed(xlim = c(14,30), ylim = c(0,15))+
  facet_wrap(~talla_ini)+
  geom_text(
    data    = dat_text,
    mapping = aes(x = 15, y = 14.5, label = label),
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