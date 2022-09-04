library(ggplot2)
library(fields)

age <- 200 # Limite del eje X en dias

# Get data from the bibliography
dirpath  <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/bib_data/'
csv_file <- paste0(dirpath, 'Moreno et al 2011 fig4.csv')
lab      <- read.table(csv_file, header = T, sep = ';')
lab$standard_length_mm <- lab$standard_length_mm/10 # Longitud estandar [de mm a cm]
lab <- subset(lab, lab$age_days <= age)
colnames(lab) <- c('t','L')

# Get DEB_out data
dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/PickSpaw1/DEB_out_s/'

functional_response <- seq(0.1, 1, 0.1)
temperature         <- c(16)
cols                <- tim.colors(n = length(temperature))

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
dat$Ww   <- dat$W_E + dat$W_V + dat$W_ER
dat <- subset(dat, dat$t <= age)

# PLOTS

# ggname <- paste0(dirpath, 'DEB_outVSMoreno2011Multicurva.png')
# ggplot(data = dat)+
#   geom_line (data = dat, mapping = aes(x = t, y = L_w, colour = f), size = 1, linetype = 'solid')+
#   geom_point(data = lab, mapping = aes(x = t, y = L), size = .9)+
#   scale_color_manual(values = tim.colors(n = length(levels(factor(dat$f)))))+
#   # facet_wrap(facets = ~f, ncol = 3, nrow = 2)+
#   labs(x = 'Time after hatching [d]', y = 'Standard Length [cm]', color = 'T [ºC]')+
#   theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
#         axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
#         axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
#         axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
#         plot.title   = element_text(face='bold', color='black', size=10, angle=0),
#         legend.text  = element_text(face='bold', color='black', size=10),
#         legend.title = element_text(face='bold', color='black', size=10),
#         legend.position   = c(0.1, 0.8),
#         legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
#         strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del titulo en facet_wrap
# ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)
# 
# ggname <- paste0(dirpath, 'DEB_outVSMoreno2011.png')
# ggplot(data = dat)+
#   geom_point(data = lab, mapping = aes(x = t, y = L), size = .75)+
#   geom_line (data = dat, mapping = aes(x = t, y = L_w, colour = temp), size = 1, linetype = 'solid')+
#   scale_color_manual(values = cols)+
#   facet_wrap(facets = ~f, ncol = 3, nrow = 2)+
#   labs(x = 'Time after hatching [d]', y = 'Standard Length [cm]', color = 'T [ºC]')+
#   theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
#         axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
#         axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
#         axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
#         plot.title   = element_text(face='bold', color='black', size=10, angle=0),
#         legend.text  = element_text(face='bold', color='black', size=10),
#         legend.title = element_text(face='bold', color='black', size=10),
#         legend.position   = c(0.03, 0.9),
#         legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
#         strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del titulo en facet_wrap
# ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)

# Plot del promedio del crecimiento + datos de paper Moreno et al 2011
mean_dat <- tapply(dat$L_w, list(dat$t, dat$temp), mean)
sd_dat   <- tapply(dat$L_w, list(dat$t, dat$temp), sd)
tim      <- rep(as.numeric(rownames(mean_dat)), times = dim(mean_dat)[2])
temp     <- rep(colnames(mean_dat), each = dim(mean_dat)[1])
sum_dat  <- data.frame(tim, temp, as.vector(mean_dat), as.vector(mean_dat)+as.vector(sd_dat), as.vector(mean_dat)-as.vector(sd_dat))
colnames(sum_dat) <- c('t', 'temp', 'mean', 'sd_up', 'sd_down')

ggname <- paste0(dirpath, 'DEB_outVSMoreno2011_promedio_f.png')
ggplot(data = dat)+
  geom_line(data = sum_dat, mapping = aes(x = t, y = mean, colour = temp), size = 2)+
  geom_line(data = sum_dat, mapping = aes(x = t, y = sd_up, colour = temp), linetype = 'dotted')+
  geom_line(data = sum_dat, mapping = aes(x = t, y = sd_down, colour = temp), linetype = 'dotted')+
  geom_point(data = lab, mapping = aes(x = t, y = L), size = 1.5)+
  scale_color_manual(values = cols)+
  labs(x = 'Time after hatching [d]', y = 'Standard Length [cm]', color = 'T [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=25, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=25, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        legend.text  = element_text(face='bold', color='black', size=25),
        legend.title = element_text(face='bold', color='black', size=25),
        legend.position   = c(0.07, 0.9),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 8, height = 8)

