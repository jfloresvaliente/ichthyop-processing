# Cargar data DEB-matlab
dat <- read.table('C:/Users/jflores/Desktop/DEB_4years_vari_Temp.txt', header = F, sep = ',')
colnames(dat) <- c('t', 10:20)
dat <- subset(dat, dat$t <= 35)
temp <- c(15,18,19)

new_dat <- NULL
for(i in 1:length(temp)){
  m <- which(names(dat) == temp[i])
  sort_dat <- cbind(dat$t, dat[,m]*10, rep(temp[i], times = dim(dat)[1]))
  new_dat <- rbind(new_dat, sort_dat)
}
new_dat <- as.data.frame(new_dat)
colnames(new_dat) <- c('t', 'Ls', 'Temp')
new_dat$Temp <- as.factor(new_dat$Temp)


# Cargar data estanques Arturo
lab <- read.table('C:/Users/jflores/Desktop/Crecimiento_E.ringens_IMAPRE.csv', header = T, sep = ',')
lab <- subset(lab, lab$Temperatura != 20)
lab$Ls <- lab$Ls/1000 # Transformacion a [mm]
lab$Temperatura <- as.factor(lab$Temperatura)

# Plot GGPLOT
p <- ggplot(data = lab)+
  geom_point(data = lab, mapping = aes(x = t, y = Ls, colour = Temperatura))+
  geom_smooth(data = lab, mapping = aes(x = t, y = Ls, colour = Temperatura), method = 'lm')+
  geom_line(data = new_dat, mapping = aes(x = t, y = Ls, colour = Temp), size = 1.5, linetype = 'dashed')+
  labs(x = 'Time in Days [d]', y = 'Length [mm]', color = 'Temperature [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=15, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=15, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=15, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=15, angle=90),
        plot.title   = element_text(face='bold', color='black', size=15, angle=0),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.15, 0.8),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'))
p
ggsave(filename = 'C:/Users/jflores/Desktop/DEB_larvaArturo.png', plot = last_plot(), width = 8, height = 8)
