library(ggplot2)
library(fields)
library(hexbin)
library(gridExtra)

# Get DEB_out data
dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEB_out/'
txt_files <- list.files(path = dirpath, pattern = 'DEB_out', full.names = T)

# E_Hb    = 0.3889  # J, Maturity threshold at birth % ouverture de la bouche
E_Hb    = 0.08    # J, Maturity threshold at birth % ouverture de la bouche % test a mano
E_Hj    = 83.22   # J, Maturity threshold at metamorphosis
E_Hp    = 42160   # J, Maturity threshold at puberty

# Auxiliary parameters
del_M   = 0.1889

functional_response <- seq(0.1, 1, 0.1)
temperature         <- 10:30

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

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
      i_b <- which(df$E_H >= E_Hb)[1]
      t_b <- df$t[i_b]
      L_b <- df$L_w[i_b]
      
      # metamorphosis index
      i_j <- which(df$E_H >= E_Hj)[1]
      t_j <- df$t[i_j]
      L_j <- df$L_w[i_j]
      
      # puberty index
      i_p <- which(df$E_H >= E_Hp)[1]
      t_p <- df$t[i_p]
      L_p <- df$L_w[i_p]
      
      val_in <- c(t_b, L_b, t_j, L_j,t_p, L_p, temperature[j], functional_response[i])
      dat <- rbind(dat, val_in)
    }
  }
  colnames(dat) <- c('age_birth', 'length_birth', 'age_metamorphosis', 'length_metamorphosis', 'age_puberty', 'length_puberty', 'temp', 'f')
  dat <- as.data.frame(dat)
  row.names(dat) <- NULL
  save(dat, file = paste0(dirpath, 'age_length_transitions.Rdata'))
}

# dat <- subset(dat, dat$f > 0.4)

# Plots en funcion de f
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


# Plots en funcion de temp
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
