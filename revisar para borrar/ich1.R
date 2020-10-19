library(ggplot2)
library(fields)

# Get data from the bibliography
dirpath  <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/bib_data/'
csv_file <- paste0(dirpath, 'CastroHernandesY1995.csv')
lab      <- read.table(csv_file, header = T, sep = ';')
lab$standard_length <- lab$standard_length/10 # de mm a cm

# Get DEB_out data
dirpath <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_encrasicolus_param/DEB_out_s/'
txt_files <- list.files(path = dirpath, pattern = 'DEB_out', full.names = T)

functional_response <- seq(1, 1.3, 0.1)
temperature         <- c(11,14)
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
dat <- subset(dat, dat$t <= 35)

dat$L_w <- (dat$V^(1/3))/del_M # cm, Physical length
