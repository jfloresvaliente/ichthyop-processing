library(ggplot2)
library(gridExtra)
library(stringr)
library(fields)

dirpath <- c('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEBoutV2/',
             'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEBoutV2/')
Temp <- c(15, 18, 19)
age  <- 35
cols                <- c('blue','green','red')

dat <- NULL
for(k in 1:length(dirpath)){
  
  files <- list.files(path = dirpath[k], pattern = '.txt', full.names = T)
  
  for(i in 1:length(Temp)){
    files_T <- which(!is.na(str_locate(string = files, pattern = paste0('T', Temp[i]))[,1]) == 1)
    files_T <- files[files_T]
    
    for(j in 1:length(files_T)){
      print(files_T[j])
      df <- read.table(files_T[j], header = T, sep = ',')
      
      # If it is necessary to calculate the physical length
      if(sum(grepl(pattern = 'Lw', x = names(df))) == 0){
        df$Lw <- df$V^(1/3)/df$delta
      }
      
      # Identificamos de que especie se trata:
      if(grepl(pattern = 'ringens', x = files[j])) df$sp <- 'E. ringens' else df$sp <- 'E. encrasicolus'
      
      df <- data.frame(df$t, df$E, df$V, df$temp, df$f, df$Lw, df$sp)
      colnames(df) <- c("t", "E", "V","temp", "f",'Lw','sp')
      
      dat <- rbind(dat, df)
    }
  }
}

dat <- subset(dat, dat$t <= age)
dat <- subset(dat, dat$f == 1)
rownames(dat) <- NULL
dat$temp <- as.factor(dat$temp)
dat$f    <- as.factor(dat$f)
dat$sp   <- as.factor(dat$sp)

#----------- GGPLOT -----------#
# 
p1 <- ggplot(data = dat, mapping = aes(x = t, y = E, colour = temp))+
  geom_line(aes(linetype = sp), size = 1.3)+
  labs(x = 'Age (d)', y = 'Reserve (J)', linetype = 'sp', colour = 'Temperature (ºC)')+
  facet_wrap(~sp)+
  scale_color_manual(values = cols)+
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
png(filename = 'C:/Users/jflores/Desktop/AgeReserveSP.png', width = 1250, height = 550, res = 120)
grid.arrange(p1, nrow = 1)
dev.off()

p2 <- ggplot(data = dat, mapping = aes(x = t, y = V, colour = temp))+
  geom_line(aes(linetype = sp), size = 1.3)+
  labs(x = 'Age (d)', y = 'Structure (cm3)', linetype = 'sp', colour = 'Temperature (ºC)')+
  facet_wrap(~sp)+
  scale_color_manual(values = cols)+
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
png(filename = 'C:/Users/jflores/Desktop/AgeStructureSP.png', width = 1250, height = 550, res = 120)
grid.arrange(p2, nrow = 1)
dev.off()

p3 <- ggplot(data = dat, mapping = aes(x = t, y = Lw, colour = temp))+
  geom_line(aes(linetype = sp), size = 1.3)+
  labs(x = 'Age (d)', y = 'Length (cm)', linetype = 'sp', colour = 'Temperature (ºC)')+
  facet_wrap(~sp)+
  scale_color_manual(values = cols)+
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
png(filename = 'C:/Users/jflores/Desktop/AgeLengthSP.png', width = 1250, height = 550, res = 120)
grid.arrange(p3, nrow = 1)
dev.off()

