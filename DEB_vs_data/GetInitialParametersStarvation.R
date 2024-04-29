# GetInitialParametersStarvation

txt_file <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/DEB/ichthyop_DEB/Engraulis_ringens_param/DEBoutV1/DEB_out.txt'
cm_tall <- c(0.5, 3)

df <- read.table(file = txt_file, header = T, sep = ',')

# If it is necessary to calculate the physical length
if(sum(grepl(pattern = 'Lw', x = names(df))) == 0){
  df$Lw <- df$V^(1/3)/df$delta
}

pars <- NULL
for(i in 1:length(cm_tall)){
  ind <- which(df$Lw > cm_tall[i])[1]
  ini_val <- df[ind,]
  
  pars <- rbind(pars, ini_val)
}

write.table(x = pars, file = 'C:/Users/jflores/Desktop/cond_ini.csv', sep = ';', row.names = F)
