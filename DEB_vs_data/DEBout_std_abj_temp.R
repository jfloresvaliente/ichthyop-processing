#=============================================================================#
# Name   : DEBout_std_abj_temp
# Author : Jorge Flores-Valiente
# Date   :
# Version:
# Aim    : Compares the growth in length (cm) of the 'std' and 'abj' models.
# URL    :
#=============================================================================#
# Type of length: standard (cm, centimeters)
# Time: age (days)

library(ggplot2)
library(fields)

age   <- 60         # X-axis limit in days
xlim  <- c(0, age)  # X-axis limits

# Get the full name of the .txt files to be read (DEBout)
dirpath <- c('C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEBoutV2/cTcase1/',
             'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEBoutV2/delta_pecquerie/cTcase1/')

# Select the desired temperatures and functional responses to be plotted
functional_response <- seq(from = 1, to = 1, by = 0.1)
temperature         <- c(15, 19, 21, 23)
cols <- c('red', 'blue') # Color for each species (model type)

# .txt file reading loop
dat <- NULL
for(k in 1:length(dirpath)){
  directory_out <- dirpath[k]
  
  for(i in 1:length(functional_response)){
    for(j in 1:length(temperature)){
      
      txt_file <- paste0(directory_out, 'DEB_outT', temperature[j],'f',functional_response[i],'.txt')
      df   <- read.table(file = txt_file, header = T, sep = ',')
      df   <- df[-dim(df)[1],] # The last row containing an inf value is deleted.
      
      # Identify the species (model type) involved:
      # if(grepl(pattern = 'ringens', x = txt_file)) df$sp <- 'E. ringens' else df$sp <- 'E. encrasicolus'
      if(grepl(pattern = 'ringens', x = txt_file)) df$sp <- 'DEBabj' else df$sp <- 'DEBstd'

      df   <- subset(df, df$t <= age) # Just the subset of data up to the desired age.
      
      # If it is necessary to calculate the physical length
      # Sometimes you'll need to give the name of the delta column,
      # which might be linked to different approaches (like Pecquerie or Jusup).
      if(sum(grepl(pattern = 'Lw', x = names(df))) == 0){
        df$Lw <- df$V^(1/3)/df$delta
        if(!grepl(pattern = 'ringens', x = txt_file)) df$Lw <- df$Lw / 1.141    # Convert 'Lw' to standard length for E. encrasicolus
      }
      
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
      
      col_ind <- c(
        which(colnames(df) == 't'),
        which(colnames(df) == 'E'),
        which(colnames(df) == 'V'),
        which(colnames(df) == 'Lw'),
        which(colnames(df) == 'delta'),
        which(colnames(df) == 'temp'),
        which(colnames(df) == 'f'),
        which(colnames(df) == 'sp'))
      df <- df[,col_ind]
      
      dat <- rbind(dat, df)
      print(txt_file)
    }
  }
}

# Data adjustments
dat$temp <- as.factor(dat$temp)             # Temperature as factor
dat$f    <- paste('f =', as.factor(dat$f))  # 'f' as a factor

# Obtain growth with a maximum f (f = 1)
f_max <- subset(dat, dat$f == max(dat$f))

# Set subplots-labels
f_max$temp <- paste(f_max$temp, 'ºC')

dat_text <- data.frame(
  label = paste0(letters[9:12], ')'),
  # label = paste0(letters[1:length(levels(factor(dat$temp)))], ')'),
  temp  = paste(levels(factor(dat$temp)), 'ºC')
)

#=============== PLOTS ===============#
# Plot1: Max growth (Lw) (solid lines) maximal functional response (f = 1)
ylim  <- c(0, 3)   # Y-axis limits
ratio <- (xlim[2]-xlim[1])/(ylim[2]-ylim[1]) # Ratio between X and Y axis for a square figure

ggname <- paste0(dirpath[2], 'DEBout_std_abj_Lw.png')
ggplot(data = f_max)+
  geom_line(data = f_max  , mapping = aes(x = t, y = Lw, colour = sp), linewidth = 1.5)+
  coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
  scale_color_manual(values = cols)+
  guides(color = guide_legend(nrow = 1))+
  labs(x = 'Age [d]', y = 'Standard Length [cm]', colour = 'Model Type')+
  facet_wrap(~temp, nrow = 1)+
  geom_text(
    data    = dat_text,
    mapping = aes(x = -1, y = (ylim[2]*96/100), label = label),
    hjust   = 0,
    vjust   = 0,
    size = 8
  )+
  theme(axis.text.x  = element_text(face='bold', color='black', size=17, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=17, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        plot.margin  = margin(2, 0.2, 0.5, 0.5, 'cm'),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.08, 1.2),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=20)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 16, height = 6)

# Plot2: Structure (V) (solid lines) maximal functional response (f = 1)
ylim  <- c(0, 0.05)   # Y-axis limits
ratio <- (xlim[2]-xlim[1])/(ylim[2]-ylim[1]) # Ratio between X and Y axis for a square figure
ggname <- paste0(dirpath[2], 'DEBout_std_abj_V.png')
ggplot(data = f_max)+
  geom_line(data = f_max  , mapping = aes(x = t, y = V, colour = sp), linewidth = 1.5)+
  coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
  scale_color_manual(values = cols)+
  guides(color = guide_legend(nrow = 1))+
  labs(x = 'Age [d]', y = expression(Structure (V, cm^3)), colour = 'Model Type')+
  facet_wrap(~temp, nrow = 1)+
  geom_text(
    data    = dat_text,
    mapping = aes(x = -1, y = (ylim[2]*96/100), label = label),
    hjust   = 0,
    vjust   = 0,
    size = 8
  )+
  theme(axis.text.x  = element_text(face='bold', color='black', size=17, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=17, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        plot.margin  = margin(2, 0.2, 0.5, 0.5, 'cm'),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.08, 1.2),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=20)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 16, height = 6)

# Plot3: Reserve (E) (solid lines) maximal functional response (f = 1)
ylim  <- c(0, 100)    # Y-axis limits
ratio <- (xlim[2]-xlim[1])/(ylim[2]-ylim[1]) # Ratio between X and Y axis for a square figure
ggname <- paste0(dirpath[2], 'DEBout_std_abj_E.png')
ggplot(data = f_max)+
  geom_line(data = f_max  , mapping = aes(x = t, y = E, colour = sp), linewidth = 1.5)+
  coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
  scale_color_manual(values = cols)+
  guides(color = guide_legend(nrow = 1))+
  labs(x = 'Age [d]', y = 'Reserve (E) [J]', colour = 'Model Type')+
  facet_wrap(~temp, nrow = 1)+
  geom_text(
    data    = dat_text,
    mapping = aes(x = -1, y = (ylim[2]*96/100), label = label),
    hjust   = 0,
    vjust   = 0,
    size = 8
  )+
  theme(axis.text.x  = element_text(face='bold', color='black', size=17, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=17, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        plot.margin  = margin(2, 0.2, 0.5, 0.5, 'cm'),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.08, 1.2),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=20)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 16, height = 6)

# Plot4: delta_M (solid lines) maximal functional response (f = 1)
ylim  <- c(0.05, 0.2)       # Y-axis limits
ratio <- (xlim[2]-xlim[1])/(ylim[2]-ylim[1]) # Ratio between X and Y axis for a square figure
ggname <- paste0(dirpath[2], 'DEBout_std_abj_delta.png')
ggplot(data = f_max)+
  geom_line(data = f_max  , mapping = aes(x = t, y = delta, colour = sp), linewidth = 1.5)+
  coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)+
  scale_color_manual(values = cols)+
  guides(color = guide_legend(nrow = 1))+
  labs(x = 'Age [d]', y = 'Shape coefficient [-]', colour = 'Model Type')+
  facet_wrap(~temp, nrow = 1)+
  geom_text(
    data    = dat_text,
    mapping = aes(x = -1, y = (ylim[2]*96/100), label = label),
    hjust   = 0,
    vjust   = 0,
    size = 8
  )+
  theme(axis.text.x  = element_text(face='bold', color='black', size=17, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=17, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=25, angle=0, margin = margin(t = 20)),
        axis.title.y = element_text(face='bold', color='black', size=25, angle=90,margin = margin(r = 20)),
        plot.title   = element_text(face='bold', color='black', size=25, angle=0),
        plot.margin  = margin(2, 0.2, 0.5, 0.5, 'cm'),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=15),
        legend.position   = c(0.08, 1.2),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'),
        strip.text        = element_text(face='bold', color='black', size=20)) # Para cambiar el tamaño del título en facet_wrap
ggsave(filename = ggname, plot = last_plot(), width = 16, height = 6)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#