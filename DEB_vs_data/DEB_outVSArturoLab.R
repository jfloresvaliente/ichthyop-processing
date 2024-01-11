library(ggplot2)
library(fields)

# Get laboratory data
dirpath  <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/engraulis_data/'
csv_file <- paste0(dirpath, 'Crecimiento_E.ringens_IMAPRE.csv')
lab      <- read.table(csv_file, header = T, sep = ',')
lab$Temperatura <- as.factor(lab$Temperatura)
lab$Ls   <- lab$Ls/10000 # de micras a cm

# Get DEB_out data
dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEB_out/'
txt_files <- list.files(path = dirpath, pattern = 'DEB_out', full.names = T)

functional_response <- seq(0.4, 1, 0.1)
temperature         <- c(15, 18, 19)
cols                <- c('blue','green','red')
# cols                <- tim.colors(n = length(temperature))

dat <- NULL
for(i in 1:length(functional_response)){
  for(j in 1:length(temperature)){
    
    txt_file <- paste0(dirpath, 'DEB_outT', temperature[j],'f',functional_response[i],'.txt')
    df <- read.table(file = txt_file, header = T, sep = ',')
    df$t <- df$t - 1 # Se resta 1 dia que corresponde al periodo de huevo.
    dat <- rbind(dat, df)
    print(txt_file)
  }
}

dat$temp <- as.factor(dat$temp)
dat$f    <- paste('f =', as.factor(dat$f))
# dat$Ww   <- dat$W_E + dat$W_V + dat$W_ER
dat <- subset(dat, dat$t <= 35)

# PLOTS
# ggname <- paste0(dirpath, 'DEB_outVSArturoLab.png')
# ggplot(data = dat)+
#   geom_line(mapping = aes(x = t, y = L_w, colour = temp), size = 1, linetype = 'solid')+
#   scale_color_manual(values = cols)+
#   facet_wrap(facets = ~f, ncol = 5, nrow = 2)+
#   labs(x = 'Time after hatching [d]', y = 'Standard Length [cm]', color = 'T [ºC]')+
#   geom_point(data = lab, mapping = aes(x = t, y = Ls, colour = Temperatura))+
#   theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
#         axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
#         axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
#         axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
#         plot.title   = element_text(face='bold', color='black', size=10, angle=0),
#         legend.text  = element_text(face='bold', color='black', size=10),
#         legend.title = element_text(face='bold', color='black', size=10),
#         legend.position   = c(0.03, 0.9),
#         legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
#         strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
# ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)

mean_dat <- tapply(dat$L_w, list(dat$t, dat$temp), mean)
sd_dat   <- tapply(dat$L_w, list(dat$t, dat$temp), sd)
tim      <- rep(as.numeric(rownames(mean_dat)), times = dim(mean_dat)[2])
temp     <- rep(colnames(mean_dat), each = dim(mean_dat)[1])
sum_dat  <- data.frame(tim, temp, as.vector(mean_dat), as.vector(mean_dat)+as.vector(sd_dat), as.vector(mean_dat)-as.vector(sd_dat))
colnames(sum_dat) <- c('t', 'temp', 'mean', 'sd_up', 'sd_down')

ggname <- paste0(dirpath, 'DEB_outVSArturoLab_promedio_f_1.0.png')
ggplot(data = dat)+
  geom_line(data = sum_dat, mapping = aes(x = t, y = mean, colour = temp), size = 2)+
  geom_line(data = sum_dat, mapping = aes(x = t, y = sd_up, colour = temp), linetype = 'dotted')+
  geom_line(data = sum_dat, mapping = aes(x = t, y = sd_down, colour = temp), linetype = 'dotted')+
  geom_point(data = lab, mapping = aes(x = t, y = Ls, colour = Temperatura), size = 1.5)+

  scale_color_manual(values = cols)+
  labs(x = 'Time after hatching [d]', y = 'Standard Length [cm]', color = 'T [ºC]')+
          theme(axis.text.x  = element_text(face='bold', color='black', size=25, angle=0),
                axis.text.y  = element_text(face='bold', color='black', size=25, angle=0),
                axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
                axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
                plot.title   = element_text(face='bold', color='black', size=25, angle=0),
                legend.text  = element_text(face='bold', color='black', size=25),
                legend.title = element_text(face='bold', color='black', size=25),
                legend.position   = c(0.09, 0.75),
                legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
                strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 8, height = 8)



T15 <- subset(dat, dat$temp == 15 & dat$f == 'f = 0.4')
plot(T15$t, T15$L_w, type = 'l')
