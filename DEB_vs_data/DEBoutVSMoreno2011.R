#=============================================================================#
# Name   : DEBoutVSMoreno2011
# Author : Jorge Flores-Valiente
# Date   :
# Version:
# Aim    : Plot DEB_out files vs Moreno 2011 data
# URL    :
#=============================================================================#
# Type of length: standard (Ls, mm)
# Time: age (days)

library(ggplot2)
library(fields)

age   <- 200        # X-axis limit in days
xlim  <- c(-3, age) # X-axis limits
ylim  <- c(0,8)     # Y-axis limits
ratio <- (xlim[2]-xlim[1])/(ylim[2]-ylim[1]) # Ratio between X and Y axis for a square figure

# Get bibliography data
dirpath   <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/bib_data/'
csv_file  <- paste0(dirpath, 'Moreno2011fig4.csv')
dat2      <- read.table(csv_file, header = T, sep = ';')
dat2$temp <- 16
dat2$L    <- dat2$L/10 # from millimeters to centimeters

# Get the full name of the .txt files to be read (DEBout)
dirpath   <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEBoutV2/cTeq1/'
txt_files <- list.files(path = dirpath, pattern = 'DEB_out', full.names = T)

# Select the desired temperatures and functional responses to be plotted
functional_response <- seq(from = 0.1, to = 1, by = 0.1)
temperature         <- c(16)
cols                <- tim.colors(n = length(temperature))

# .txt file reading loop
dat <- NULL
for(i in 1:length(functional_response)){
  for(j in 1:length(temperature)){
    
    txt_file <- paste0(dirpath, 'DEB_outT', temperature[j],'f',functional_response[i],'.txt')

    df   <- read.table(file = txt_file, header = T, sep = ',')
    # df$t <- df$t - 2 # Remove 2 days corresponding to the egg period (E. ringens)
    # If previous line is uncommented, then change ylabel from 'Age [d]' to 'Time since hatching [d]'
    
    df   <- subset(df, df$t <= age) # Just the subset of data up to the desired age.
    
    # If it is necessary to calculate the physical length
    # Sometimes you'll need to give the name of the delta column,
    # which might be linked to different approaches (like Pecquerie or Jusup).
    if(sum(grepl(pattern = 'Lw', x = names(df))) == 0) df$Lw <- df$V^(1/3)/df$delta
    
    # # If it is necessary to calculate the weight
    # if(sum(grepl(pattern = 'Ww', x = names(df))) == 0){
    #   #------- Get wet weight -------#
    #   d_V  <- 0.23   # g/cm^3, specific density of structure (dry weight)
    #   mu_V <- 500000 # J/mol, specific chemical potential of structure
    #   mu_E <- 550000 # J/mol, specific chemical potential of reserve
    #   w_V  <- 23.9   # g/mol, molecular weight of structure
    #   w_E  <- 23.9   # g/mol, molecular weight of reserve
    #   c_w  <- 0.756  # (c_w * W_w = total water weight)
    #   
    #   W_V        <- d_V*df$V
    #   W_E        <- (w_E/mu_E)*df$E
    #   W_ER       <- (w_E/mu_E)*df$E_R
    #   Dry_weight <- cbind(W_V, W_E, W_ER) # g, Dry weight
    #   Wet_weight <- Dry_weight/(1 - c_w)  # g, Wet weight
    #   df$Ww = apply(Wet_weight, 1, sum) # g, Weight
    # }
    
    dat <- rbind(dat, df)
    print(txt_file)
  }
}

# Data adjustments
dat$temp <- as.factor(dat$temp)             # Temperature as factor
dat$f    <- paste('f =', as.factor(dat$f))  # 'f' as a factor

# Compute the growth mean and standard deviation for each time interval
mean_dat <- tapply(dat$Lw, list(dat$t, dat$temp), mean)
sd_dat   <- tapply(dat$Lw, list(dat$t, dat$temp), sd)
tim      <- rep(as.numeric(rownames(mean_dat)), times = dim(mean_dat)[2]) # generate a new time vector for the new dataframe
temp     <- rep(colnames(mean_dat), each = dim(mean_dat)[1])
sum_dat  <- data.frame(tim, temp, as.vector(mean_dat),
                       as.vector(mean_dat)+as.vector(sd_dat), as.vector(mean_dat)-as.vector(sd_dat))
colnames(sum_dat) <- c('t', 'temp', 'mean', 'sd_up', 'sd_down')

# Obtain growth with a maximum f
f_max <- subset(dat, dat$f == max(dat$f))

# Obtain growth with a minimum f
f_min <- subset(dat, dat$f == min(dat$f))

#=============== PLOTS ===============#

# Plot1: Mean (solid lines) of the different functional responses (f) and
# their standard deviation (sd, dotted lines) at each time step.
ggname <- paste0(dirpath, 'DEBoutVSMoreno2011fMeanSD.png')
ggplot(data = dat)+
  geom_line(data = sum_dat, mapping = aes(x = t, y = mean, colour = temp), linewidth = 2)+
  geom_line(data = sum_dat, mapping = aes(x = t, y = sd_up, colour = temp), linetype = 'dotted')+
  geom_line(data = sum_dat, mapping = aes(x = t, y = sd_down, colour = temp), linetype = 'dotted')+
  geom_point(data = dat2, mapping = aes(x = t, y = L), size = 1.5)+
  coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
  scale_color_manual(values = cols)+
  labs(x = 'Age [d]', y = 'Standard Length [cm]', color = 'T [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=25, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=25, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        legend.text  = element_text(face='bold', color='black', size=25),
        legend.title = element_text(face='bold', color='black', size=25),
        legend.position   = c(0.12, 0.85),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 8, height = 8)

# Plot2: Mean (solid lines) of the different functional responses (f) and
# maximum and minimun funcional response (dotted lines) values at each time step.
ggname <- paste0(dirpath, 'DEBoutVSMoreno2011fMaxMin.png')
ggplot(data = dat)+
  geom_line(data = sum_dat, mapping = aes(x = t, y = mean, colour = temp), linewidth = 2)+
  geom_line(data = f_max  , mapping = aes(x = t, y = Lw  , colour = temp), linetype = 'dotted')+
  geom_line(data = f_min  , mapping = aes(x = t, y = Lw  , colour = temp), linetype = 'dotted')+
  geom_point(data = dat2, mapping = aes(x = t, y = L), size = 1.5)+
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
        legend.position   = c(0.12, 0.85),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 8, height = 8)

# Plot3: Max growth (solid lines) maximal functional response (f = 1)
ggname <- paste0(dirpath, 'DEBoutVSMoreno2011f1.png')
ggplot(data = dat)+
  geom_line(data = f_max  , mapping = aes(x = t, y = Lw  , colour = temp), linewidth = 2)+
  geom_point(data = dat2, mapping = aes(x = t, y = L), size = 1.5)+
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
        legend.position   = c(0.12, 0.85),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 8, height = 8)

# Plot4: Max growth (solid lines) maximal functional response (f = 1)
ggname <- paste0(dirpath, 'DEBoutVSMoreno2011f.png')
ggplot(data = dat)+
  geom_line(data = dat, mapping = aes(x = t, y = Lw  , colour = temp), size = 2)+
  geom_point(data = dat2, mapping = aes(x = t, y = L), size = 1.5)+
  coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
  facet_wrap(~f, nrow = 2)+
  scale_color_manual(values = cols)+
  guides(color = guide_legend(nrow = 1))+
  labs(x = 'Time after hatching [d]', y = 'Standard Length [cm]', color = 'T [ºC]')+
  theme(axis.text.x  = element_text(face='bold', color='black', size=10, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=10, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=20, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=20, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=10, angle=0),
        plot.margin  = margin(2, 0.2, 0.5, 0.5, 'cm'),
        legend.text  = element_text(face='bold', color='black', size=10),
        legend.title = element_text(face='bold', color='black', size=10),
        legend.position   = c(0.07, 1.1),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=10)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 14, height = 8)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#