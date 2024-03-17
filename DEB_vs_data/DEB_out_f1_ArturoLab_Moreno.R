#=============================================================================#
# Name   : DEB_out_f1_ArturoLab_Moreno
# Author : Jorge Flores-Valiente
# Date   :
# Version:
# Aim    : Plot DEB_out files vs Arturo laboratory data
# URL    :
#=============================================================================#
library(ggplot2)
library(fields)

age <- 40 # Limite del eje X en dias

# Get laboratory data
dirpath  <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/engraulis_data/'
csv_file <- paste0(dirpath, 'Crecimiento_E.ringens_IMAPRE.csv')
lab      <- read.table(csv_file, header = T, sep = ',')
lab$Ls   <- lab$Ls/10000 # de micras a cm
lab1 <- cbind(lab$t, lab$Ls, lab$Temperatura)
# lab1$Temperatura <- as.factor(lab$Temperatura)
colnames(lab1) <- c('t', 'Ls', 'Temp')
lab1 <- as.data.frame(lab1)

# Get data from the bibliography
dirpath  <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/bib_data/'
csv_file <- paste0(dirpath, 'Moreno et al 2011 fig4.csv')
lab      <- read.table(csv_file, header = T, sep = ';')
lab$standard_length_mm <- lab$standard_length_mm/10 # Longitud estandar [de mm a cm]
lab <- subset(lab, lab$age_days <= age)
lab$Temperatura <- 16
colnames(lab) <- c('t','Ls','Temp')
lab2 <- lab

lab <- rbind(lab1, lab2)
lab$Temp <- as.factor(lab$Temp)

# Get DEB_out data
dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEBout/'
txt_files <- list.files(path = dirpath, pattern = 'DEB_out', full.names = T)

functional_response <- seq(from = 1, to = 1, by = 0.1)
temperature         <- c(15, 16, 18, 19)
cols                <- c('blue', 'black', 'green', 'red')

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

# Adjustments to the data
dat$temp <- as.factor(dat$temp) # Temperature as factor
dat$f    <- paste('f =', as.factor(dat$f)) # A subset of data at a given age.

#------------- PLOTS -------------#

# Plot1: Mean (solid lines) of the different functional responses (f) and
# their standard deviation (sd, dotted lines) at each time step.
ggname <- paste0(dirpath, 'DEB_out_f1_ArturoLab_Moreno.png')
ggplot(data = dat)+
  geom_line(data = dat, mapping = aes(x = t, y = Lw  , colour = temp), size = 2)+
  geom_point(data = lab, mapping = aes(x = t, y = Ls, colour = Temp), size = 1.5)+
  coord_fixed(xlim = c(-3, 50), ylim = c(0,2.5), ratio = 15.2)+
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
