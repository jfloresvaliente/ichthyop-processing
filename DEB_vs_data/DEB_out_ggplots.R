library(ggplot2)
library(fields)

dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEB_out_t/'
txt_files <- list.files(path = dirpath, pattern = 'DEB_out', full.names = T)

functional_response <- c(0.1, 0.3, 0.5, 0.7, 1)
temperature         <- seq(10, 30, 3)
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

#================= PLOTS =================#

# t_Lw_by_f
ggname <- paste0(dirpath, 't_Lw_by_f.png')
ggplot(data = dat)+
  geom_line(mapping = aes(x = t, y = L_w, colour = temp), size = 1, linetype = 'solid')+
  scale_color_manual(values = cols)+
  facet_wrap(facets = ~f, ncol = 3, nrow = 2)+
  labs(x = 'Time in Days [d]', y = 'Length [cm]', color = 'T [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(0.8, 0.3),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)

# t_Ww_by_f
ggname <- paste0(dirpath, 't_Ww_by_f.png')
ggplot(data = dat)+
  geom_line(mapping = aes(x = t, y = Ww, colour = temp), size = 1, linetype = 'solid')+
  scale_color_manual(values = cols)+
  facet_wrap(facets = ~f, ncol = 3, nrow = 2)+
  labs(x = 'Time in Days [d]', y = 'Wet Weight [g]', color = 'T [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(0.8, 0.3),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)


# Lw_Ww_by_f
ggname <- paste0(dirpath, 'Lw_Ww_by_f.png')
ggplot(data = dat)+
  geom_line(mapping = aes(x = L_w, y = Ww, colour = temp), size = 1, linetype = 'solid')+
  scale_color_manual(values = cols)+
  facet_wrap(facets = ~f, ncol = 3, nrow = 2)+
  labs(x = 'Length [cm]', y = 'Wet Weight [g]', color = 'T [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(0.8, 0.3),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)

# t_E_by_f
ggname <- paste0(dirpath, 't_E_by_f.png')
ggplot(data = dat)+
  geom_line(mapping = aes(x = t, y = E, colour = temp), size = 1, linetype = 'solid')+
  scale_color_manual(values = cols)+
  facet_wrap(facets = ~f, ncol = 3, nrow = 2)+
  labs(x = 'Time in Days [d]', y = 'Reserve E (J)', color = 'T [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(0.8, 0.3),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)

# t_V_by_f
ggname <- paste0(dirpath, 't_V_by_f.png')
ggplot(data = dat)+
  geom_line(mapping = aes(x = t, y = V, colour = temp), size = 1, linetype = 'solid')+
  scale_color_manual(values = cols)+
  facet_wrap(facets = ~f, ncol = 3, nrow = 2)+
  labs(x = 'Time in Days [d]', y = 'Structure V (cm^3)', color = 'T [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(0.8, 0.3),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)

# t_E_R_by_f
ggname <- paste0(dirpath, 't_E_R_by_f.png')
ggplot(data = dat)+
  geom_line(mapping = aes(x = t, y = E_R, colour = temp), size = 1, linetype = 'solid')+
  scale_color_manual(values = cols)+
  facet_wrap(facets = ~f, ncol = 3, nrow = 2)+
  labs(x = 'Time in Days [d]', y = 'E_R (J)', color = 'T [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=10, angle=90),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(0.8, 0.3),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)
