library(ggplot2)
library(fields)

dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/VB_data/'
dat <- read.csv(paste0(dirpath, 'FishBaseEringensVB.csv'), sep = ';')
# dat <- dat[c(1,5,8,10),]

# t0   <- -0.14 # Marzloff 2009
t0   <- 0
# Linf <- 20.5
# k    <- 0.86
t_VB <- seq(1, 4*365, 1)/365
ind_t <- seq(from = 365, to = 1460, by = 365)

data_points <- NULL
df <- NULL
for(i in 1:dim(dat)[1]){
  # L  <- Linf * (1 - exp(-k * (t_VB - t0)))
  L  <- dat$Loo[i] * (1 - exp(-dat$K[i] * (t_VB - t0)))
  bib <- cbind(t_VB, L, dat$Temp[i], dat$Loo[i], dat$K[i], t0)
  df <- rbind(df, bib)
  
  p <- cbind(t_VB[ind_t], L[ind_t], dat$Temp[i], dat$Loo[i], dat$K[i], t0, dat$DataReference[i])
  data_points <- rbind(data_points, p)
}

colnames(df) <- c('t','L', 'temp', 'Loo', 'k', 't0')
df <- as.data.frame(df)
df$temp <- as.factor(df$temp)
cols <- tim.colors(n = length(levels(factor(df$temp))))

colnames(data_points) <- c('t','L', 'temp', 'Loo', 'k', 't0', 'bib')
data_points <- as.data.frame(data_points)
data_points$temp <- as.factor(data_points$temp)
data_points$t <- round(as.numeric(data_points$t))

ggplot(data = df)+
  # geom_line(mapping = aes(x = t, y = L, colour = temp), size = 2)+
  geom_point(mapping = aes(x = t, y = L, colour = temp), size = .5)+
  # coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
  scale_color_manual(values = cols)+
  # scale_color_continuous()+
  # expand_limits(x = 0, y = 0)
  # scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  labs(x = 'Age [years]', y = 'Total Length [cm]', color = 'T [ºC]')+
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

ggplot(data = data_points)+
  # geom_line(mapping = aes(x = t, y = L, colour = temp), size = 2)+
  geom_point(mapping = aes(x = t, y = L, colour = temp), size = 2)+
  # coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
  scale_color_manual(values = cols)+
  # scale_color_continuous()+
  # expand_limits(x = 0, y = 0)
  # scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  labs(x = 'Age [years]', y = 'Total Length [cm]', color = 'T [ºC]')+
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


rownames(data_points) <- NULL
write.csv(x = data_points, file = 'C:/Users/jflores/Desktop/VB_datapoints.csv', row.names = F)
