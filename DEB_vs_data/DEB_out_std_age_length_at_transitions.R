library(ggplot2)
library(ggplot2)
library(fields)
library(hexbin)
library(gridExtra)

# Get DEB_out data
dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEB_out_s/'
txt_files <- list.files(path = dirpath, pattern = 'DEB_out', full.names = T)

L_wb  = 0.25    # cm, total length at mouth opening --> ?? total or fork length? [add my pet]
L_wp  = 9.077   # cm, total length at puberty --> ?? guess [add my pet]

# Auxiliary parameters
del_M   = 0     # 1 = Total Length | 0 = Standard Length
del_M_t = 0.154 # - , shape coefficient (Total Length)[Pethybridge et al 2013]
del_M_s = 0.166 # - , This value produces a standard length of 1.51 cm less than the total length, which is observed in ichthyometer figures.

if(del_M == 0) del_M = del_M_t else del_M = del_M_s

functional_response <- seq(0.1, 1, 0.1)
temperature         <- 10:30

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

# % Compound parameters
V_b = (L_wb * del_M)^3 # cm^3, structural volume at birth (first feeding)
V_p = (L_wp * del_M)^3 # cm^3, structural volume at puberty

if(file.exists(paste0(dirpath, 'age_length_transitions.Rdata'))){
  load(paste0(dirpath, 'age_length_transitions.Rdata'))
}else{
  dat <- NULL
  for(i in 1:length(functional_response)){
    for(j in 1:length(temperature)){
      
      txt_file <- paste0(dirpath, 'DEB_outT', temperature[j],'f',functional_response[i],'.txt')
      df <- read.table(file = txt_file, header = T, sep = ',')
      print(txt_file)
      df$t <- df$t #- 1 # Se resta 1 dia que corresponde al periodo de huevo.
      
      # birth index
      i_b <- which(df$V >= V_b)[1]
      t_b <- df$t[i_b]
      L_b <- df$L_w[i_b]
      
      # metamorphosis index
      i_j <- which(df$L_w >= 1.4)[1] # hay que calcular un valor DEBabj para la metamorfosis
      t_j <- df$t[i_j]
      L_j <- df$L_w[i_j]
      
      # puberty index
      i_p <- which(df$V >= V_p)[1]
      t_p <- df$t[i_p]
      L_p <- df$L_w[i_p]
      
      val_in <- c(t_b, L_b, t_j, L_j, t_p, L_p, temperature[j], functional_response[i])
      dat <- rbind(dat, val_in)
    }
  }
  colnames(dat) <- c('age_birth', 'length_birth', 'age_metamorphosis', 'length_metamorphosis', 'age_puberty', 'length_puberty', 'temp', 'f')
  dat <- as.data.frame(dat)
  row.names(dat) <- NULL
  save(dat, file = paste0(dirpath, 'age_length_transitions.Rdata'))
}

# Plots en function de f
p1 <- ggplot(data = dat, aes(x = factor(f), y = age_birth))+
  geom_boxplot(outlier.shape = NA)

p2 <- ggplot(data = dat, aes(x = factor(f), y = length_birth))+
  geom_boxplot(outlier.shape = NA)

p3 <- ggplot(data = dat, aes(x = factor(f), y = age_metamorphosis))+
  geom_boxplot(outlier.shape = NA)

p4 <- ggplot(data = dat, aes(x = factor(f), y = length_metamorphosis))+
  geom_boxplot(outlier.shape = NA)

p5 <- ggplot(data = dat, aes(x = factor(f), y = age_puberty))+
  geom_boxplot(outlier.shape = NA)

p6 <- ggplot(data = dat, aes(x = factor(f), y = length_puberty))+
  geom_boxplot(outlier.shape = NA)

png(filename = paste0(dirpath, 'f_age.png'), width = 850, height = 850, res = 120)
grid.arrange(p1, p3, p5, nrow = 3)
dev.off()

png(filename = paste0(dirpath, 'f_length.png'), width = 850, height = 850, res = 120)
grid.arrange(p2, p4, p6, nrow = 3)
dev.off()


# Plots en function de temp
p1 <- ggplot(data = dat, aes(x = factor(temp), y = age_birth))+
  geom_boxplot(outlier.shape = NA)

p2 <- ggplot(data = dat, aes(x = factor(temp), y = length_birth))+
  geom_boxplot(outlier.shape = NA)

p3 <- ggplot(data = dat, aes(x = factor(temp), y = age_metamorphosis))+
  geom_boxplot(outlier.shape = NA)

p4 <- ggplot(data = dat, aes(x = factor(temp), y = length_metamorphosis))+
  geom_boxplot(outlier.shape = NA)

p5 <- ggplot(data = dat, aes(x = factor(temp), y = age_puberty))+
  geom_boxplot(outlier.shape = NA)

p6 <- ggplot(data = dat, aes(x = factor(temp), y = length_puberty))+
  geom_boxplot(outlier.shape = NA)

png(filename = paste0(dirpath, 'temp_age.png'), width = 850, height = 850, res = 120)
grid.arrange(p1, p3, p5, nrow = 3)
dev.off()

png(filename = paste0(dirpath, 'temp_length.png'), width = 850, height = 850, res = 120)
grid.arrange(p2, p4, p6, nrow = 3)
dev.off()
