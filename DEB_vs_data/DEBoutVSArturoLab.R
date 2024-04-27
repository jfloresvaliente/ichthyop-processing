#=============================================================================#
# Name   : DEBoutVSArturoLab
# Author : Jorge Flores-Valiente
# Date   :
# Version:
# Aim    : Plot DEB_out files vs Arturo laboratory data
# URL    :
#=============================================================================#
# Type of length: standard (Ls, microns)
# Time: age (days)

library(ggplot2)
library(fields)

age   <- 35         # X-axis limit in days
xlim  <- c(-3, 35)  # X-axis limits
ylim  <- c(0,2.5)   # Y-axis limits
ratio <- 15.2       # Ratio between X and Y axis

# Get laboratory data
dirpath   <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/engraulis_data/'
csv_file  <- paste0(dirpath, 'CrecimientoEringensIMAPRE.csv')
dat2      <- read.table(csv_file, header = T, sep = ',')
dat2$temp <- as.factor(dat2$temp)
dat2$L    <- dat2$L/10000 # from microns to cm

# Get DEB_out data
dirpath   <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEBoutV2/'
txt_files <- list.files(path = dirpath, pattern = 'DEB_out', full.names = T)

functional_response <- seq(from = 0.1, to = 1, by = 0.1)
temperature         <- c(15, 18, 19)
cols                <- c('blue','green','red')

dat <- NULL
for(i in 1:length(functional_response)){
  for(j in 1:length(temperature)){

    txt_file <- paste0(dirpath, 'DEB_outT', temperature[j],'f',functional_response[i],'.txt')
    df   <- read.table(file = txt_file, header = T, sep = ',')
    df$t <- df$t - 2 # Se resta 2 dias que corresponde al periodo de huevo (E. ringens)
    df   <- subset(df, df$t <= age)

    # If it is necessary to calculate the physical length
    if(sum(grepl(pattern = 'Lw', x = names(df))) == 0){
      df$Lw <- df$V^(1/3)/df$delta
    }

    # If it is necessary to calculate the weight
    if(sum(grepl(pattern = 'Ww', x = names(df))) == 0){
      #------- Get wet weight -------#
      d_V  <- 0.23   # g/cm^3, specific density of structure (dry weight)
      mu_V <- 500000 # J/mol, specific chemical potential of structure
      mu_E <- 550000 # J/mol, specific chemical potential of reserve
      w_V  <- 23.9   # g/mol, molecular weight of structure
      w_E  <- 23.9   # g/mol, molecular weight of reserve
      c_w  <- 0.756  # (c_w * W_w = total water weight)

      W_V        <- d_V*df$V
      W_E        <- (w_E/mu_E)*df$E
      W_ER       <- (w_E/mu_E)*df$E_R
      Dry_weight <- cbind(W_V, W_E, W_ER) # g, Dry weight
      Wet_weight <- Dry_weight/(1 - c_w)  # g, Wet weight
      df$Ww = apply(Wet_weight, 1, sum) # g, Weight
    }

    dat <- rbind(dat, df)
    print(txt_file)
  }
}

# Data adjustments
dat$temp <- as.factor(dat$temp)             # Temperature as factor
dat$f    <- paste('f =', as.factor(dat$f))  # 'f' as a factor

# Compute the mean and standard deviation of growth for each time interval
mean_dat <- tapply(dat$Lw, list(dat$t, dat$temp), mean)
sd_dat   <- tapply(dat$Lw, list(dat$t, dat$temp), sd)
tim      <- rep(as.numeric(rownames(mean_dat)), times = dim(mean_dat)[2])
temp     <- rep(colnames(mean_dat), each = dim(mean_dat)[1])
sum_dat  <- data.frame(tim, temp, as.vector(mean_dat), as.vector(mean_dat)+as.vector(sd_dat), as.vector(mean_dat)-as.vector(sd_dat))
colnames(sum_dat) <- c('t', 'temp', 'mean', 'sd_up', 'sd_down')

# Obtain growth with a maximum f
f_max <- subset(dat, dat$f == max(dat$f))

# Obtain growth with a minimum f
f_min <- subset(dat, dat$f == min(dat$f))

#------------- PLOTS -------------#

# Plot1: Mean (solid lines) of the different functional responses (f) and
# their standard deviation (sd, dotted lines) at each time step.
ggname <- paste0(dirpath, 'DEBoutVSArturoLabfMeanSD.png')
ggplot(data = dat)+
  geom_line(data = sum_dat, mapping = aes(x = t, y = mean,    colour = temp), size = 2)+
  geom_line(data = sum_dat, mapping = aes(x = t, y = sd_up,   colour = temp), linetype = 'dotted')+
  geom_line(data = sum_dat, mapping = aes(x = t, y = sd_down, colour = temp), linetype = 'dotted')+
  geom_point(data = dat2, mapping = aes(x = t, y = L, colour = temp), size = 1.5)+
  coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
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

# Plot2: Mean (solid lines) of the different functional responses (f) and
# maximum and minimun funcional response (dotted lines) values at each time step.
ggname <- paste0(dirpath, 'DEBoutVSArturoLabfMaxMin.png')
ggplot(data = dat)+
  geom_line(data = sum_dat, mapping = aes(x = t, y = mean, colour = temp), size = 2)+
  geom_line(data = f_max  , mapping = aes(x = t, y = Lw  , colour = temp), linetype = 'dotted')+
  geom_line(data = f_min  , mapping = aes(x = t, y = Lw  , colour = temp), linetype = 'dotted')+
  geom_point(data = dat2, mapping = aes(x = t, y = L, colour = temp), size = 1.5)+
  coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
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

# Plot3: Max growth (solid lines) maximal functional response (f = 1)
ggname <- paste0(dirpath, 'DEBoutVSArturoLabf1.png')
ggplot(data = dat)+
  geom_line(data = f_max  , mapping = aes(x = t, y = Lw  , colour = temp), size = 2)+
  geom_point(data = dat2, mapping = aes(x = t, y = L, colour = temp), size = 1.5)+
  coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
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

# Plot4: Max growth (solid lines) maximal functional response (f = 1)
ggname <- paste0(dirpath, 'DEBoutVSArturoLabf.png')
ggplot(data = dat)+
  geom_line(data = dat, mapping = aes(x = t, y = Lw  , colour = temp), size = 2)+
  geom_point(data = dat2, mapping = aes(x = t, y = L, colour = temp), size = 1.5)+
  coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
  facet_wrap(~f)+
  scale_color_manual(values = cols)+
  labs(x = 'Time after hatching [d]', y = 'Standard Length [cm]', color = 'T [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=10, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=10, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(0.7, 0.09),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 12, height = 8)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#