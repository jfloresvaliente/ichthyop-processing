library(ggplot2)
library(gridExtra)
library(stringr)
library(fields)

dirpath <- c('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEBoutV4/',
             'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEBoutV4/')
Lw <- c(.5, 3)

dat <- NULL
for(k in 1:length(dirpath)){
  for(i in 1:length(Lw)){
    directory_out <- paste0(dirpath[k], 'Lw',Lw[i], '/')
    files_f0 <- list.files(path = directory_out, pattern = 'f0.txt', full.names = T)
    
    for(j in 1:length(files_f0)){
      print(files_f0[j])
      df <- read.table(files_f0[j], header = T, sep = ',')
      
      # Identificamos de que especie se trata:
      if(grepl(pattern = 'ringens', x = files_f0[j])) df$sp <- 'E. ringens' else df$sp <- 'E. encrasicolus'
      
      # En caso de inanicion
      ind <- which(df$E <= 0)[1] # Elegir el momento antes de que E_0 sea negativo = muerte
      df <- df[1:ind,]
      
      # encontrar la talla inicial deseada
      ind_talla <- which(df$Lw >= Lw[i])[1] - 1 # Elegir indice de  tiempo en que alcanza la talla inicial
      df <- df[-c(1:ind_talla), ]
      
      # New data frame
      age_talla      <- df$t[1]
      age_starvation <- df$t[dim(df)[1]]
      t_starvation   <- age_starvation - age_talla
      Lw_ini        <- Lw[i]
      Lw_end        <- df$Lw[dim(df)[1]]
      temp           <- df$temp[1]
      sp             <- df$sp[1]
      
      df <- cbind(age_talla, age_starvation, t_starvation, Lw_ini, Lw_end, temp, sp)
      colnames(df) <- c('age_talla', 'age_starvation', 't_starvation', 'Lw_ini', 'Lw_end', 'temp', 'sp')
      dat <- rbind(dat, df)
    }
  }
}


dat <- as.data.frame(dat)
dat$Lw_ini <- as.factor(dat$Lw_ini)
dat$t_starvation <- as.numeric(dat$t_starvation)
dat$sp <- as.factor(dat$sp)
dat$temp <- as.numeric(dat$temp)

dat <- subset(dat, dat$temp >= 14)

# colnames(dat) <- c('age_talla', 'age_starvation', 't_starvation', 'Initial\nStandard Length\n(cm)', 'Lw_end', 'temp', 'sp')
rownames(dat) <- NULL

#----------- GGPLOT -----------#
# 
p1 <- ggplot(data = dat, mapping = aes(x = temp, y = t_starvation, colour = sp))+
  geom_line(aes(linetype = Lw_ini), size = 1.3)+
  labs(x = 'Temperature (ºC)', y = 'Time to starvation (d)', linetype = 'Initial\nStandard Length\n(cm)', colour = 'sp')+
  # geom_point(aes(shape = Lw_ini), size = 1.3)+
  # labs(x = 'Temperature (ºC)', y = 'Time to starvation (d)', shape = 'Initial\nStandard Length\n(cm)', colour = 'sp')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=13, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=13, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=15, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=15, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        plot.margin  = unit(c(.5,4,.5,.5),'cm'),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(1.1, .75),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face = 'bold', color = 'black', size = 12)) # Para cambiar el tamaño del título en facet_wrap
png(filename = 'C:/Users/jflores/Desktop/t_starvation_f0.png', width = 1250, height = 550, res = 120)
grid.arrange(p1, nrow = 1)
dev.off()

p2 <- ggplot(data = dat, mapping = aes(x = temp, y = t_starvation, colour = sp))+
  geom_line(aes(linetype = Lw_ini), size = 1.3)+
  labs(x = 'Temperature (ºC)', y = 'Time to starvation (d)', linetype = 'Initial\nStandard Length\n(cm)', colour = 'sp')+
  facet_wrap(~Lw_ini)+
  # geom_point(aes(shape = Lw_ini), size = 1.3)+
  # labs(x = 'Temperature (ºC)', y = 'Time to starvation (d)', shape = 'Initial\nStandard Length\n(cm)', colour = 'sp')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=13, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=13, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=15, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=15, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        plot.margin  = unit(c(.5,4,.5,.5),'cm'),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(1.1, .75),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face = 'bold', color = 'black', size = 12)) # Para cambiar el tamaño del título en facet_wrap
png(filename = 'C:/Users/jflores/Desktop/t_starvation_f0_V2.png', width = 1250, height = 550, res = 120)
grid.arrange(p2, nrow = 1)
dev.off()