#=============================================================================#
# Name   : DEBoutStateVar2sp
# Author : Jorge Flores-Valiente
# Date   :
# Version:
# Aim    : Plot DEB_out state variables for 2 SP
# URL    :
#=============================================================================#
# Type of length: standard (Ls, microns)
# Time: age (days)

library(ggplot2)
library(fields)
library(dplyr)

age   <- 35         # X-axis limit in days
# xlim  <- c(-3, age)  # X-axis limits
# ylim  <- c(0,2.5)   # Y-axis limits
# ratio <- 15.2       # Ratio between X and Y axis

# Get DEB_out data
dirpath <- c('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEBoutV2/cTcase1/',
             'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEBoutV2/cTcase1/')
txt_files <- list.files(path = dirpath, pattern = 'DEB_out', full.names = T)

functional_response <- seq(from = 0.1, to = 1, by = 0.1)
temperature         <- c(15, 18, 19)
cols                <- c('blue','green','red')

dat <- NULL
for(k in 1:length(dirpath)){
  directory_out <- dirpath[k]
  
  for(i in 1:length(functional_response)){
    for(j in 1:length(temperature)){
      
      txt_file <- paste0(directory_out, 'DEB_outT', temperature[j],'f',functional_response[i],'.txt')
      df   <- read.table(file = txt_file, header = T, sep = ',')
      
      # Identificamos de que especie se trata:
      # if(grepl(pattern = 'ringens', x = txt_file)) df$sp <- 'E. ringens' else df$sp <- 'E. encrasicolus'
      if(grepl(pattern = 'ringens', x = txt_file)) df$sp <- 'DEBabj' else df$sp <- 'DEBstd'
      # df$t <- df$t - 2 # Se resta 2 dias que corresponde al periodo de huevo (E. ringens)
      
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
      
      col_ind <- c(
        which(colnames(df) == 't'),
        which(colnames(df) == 'E'),
        which(colnames(df) == 'V'),
        which(colnames(df) == 'Lw'),
        which(colnames(df) == 'temp'),
        which(colnames(df) == 'f'),
        which(colnames(df) == 'sp')
        )
      df <- df[,col_ind]
      
      dat <- rbind(dat, df)
      print(txt_file)
    }
  }
}

# Data adjustments
dat$temp <- as.factor(dat$temp)             # Temperature as factor
dat$f    <- paste('f =', as.factor(dat$f))  # 'f' as a factor

# Obtain growth with a maximum f
f_max <- subset(dat, dat$f == max(dat$f))

#------------- PLOTS -------------#

# Plot: Max growth (solid lines) maximal functional response (f = 1)
f_max$temp <- paste(f_max$temp, 'ºC')
cols <- rep(c('red', 'blue'), 3)

dat_text <- data.frame(
  label = c('a)', 'b)', 'c)'),
  temp   = c('15 ºC', '18 ºC', '19 ºC')
)

ggname <- paste0('C:/Users/jflores/Desktop/DEBoutStateVar2sp_E.png')
ggplot(data = f_max)+
  geom_line(data = f_max  , mapping = aes(x = t, y = E, colour = sp), linewidth = 1.5)+
  # coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
  scale_color_manual(values = cols)+
  scale_linetype_manual(values=c('twodash', 'solid'))+
  labs(x = 'Age [d]', y = 'Reserve [J]', colour = 'Model Type')+
  facet_wrap(~temp)+
  # geom_text(
  #   data    = dat_text,
  #   mapping = aes(x = -1, y = 175, label = label),
  #   hjust   = 0,
  #   vjust   = 0,
  #   size = 8
  # )+
  theme(axis.text.x  = element_text(face='bold', color='black', size=20, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=20, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.08, 0.7),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=20)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 16, height = 8)

ggname <- paste0('C:/Users/jflores/Desktop/DEBoutStateVar2sp_V.png')
ggplot(data = f_max)+
  geom_line(data = f_max  , mapping = aes(x = t, y = V, colour = sp), linewidth = 1.5)+
  # coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
  scale_color_manual(values = cols)+
  scale_linetype_manual(values=c('twodash', 'solid'))+
  labs(x = 'Age [d]', y = 'Structure (cm^3)', colour = 'Model Type')+
  facet_wrap(~temp)+
  # geom_text(
  #   data    = dat_text,
  #   mapping = aes(x = -1, y = 0.075, label = label),
  #   hjust   = 0,
  #   vjust   = 0,
  #   size = 8
  # )+
  theme(axis.text.x  = element_text(face='bold', color='black', size=20, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=20, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.08, 0.7),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=20)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 16, height = 8)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#