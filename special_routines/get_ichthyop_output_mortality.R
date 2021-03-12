dirpath <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/10kmparent/DEB/k_x1.6_90days/out/results/'

# Read Ichthyop output in .csv format
dat <- read.table(paste0(dirpath, 'ichthyop_output.csv'), header = T, sep = ';')

# Read Data at Recruitment Age
load(paste0(dirpath,'data_atRecruitmentAge.Rdata'))
dat_age <- df; rm(df)

# Calcular el valor de sobrevivencia para cada fila de datos de 'dat'
new_dat <- NULL
for(i in 1:dim(dat)[1]){
  
  dat_row <- dat[i,]
  row_sub <- subset(dat_age,
                      dat_age$Year         == dat_row$Year
                    & dat_age$Month        == dat_row$Month
                    & dat_age$t_x          == dat_row$t_x
                    & dat_age$ReleaseDepth == as.character(dat_row$Depth)
                    & dat_age$ReleaseBathy == as.character(dat_row$Bathy)
                    & dat_age$Zone_name    == as.character(dat_row$Zone_name)

  )

  N_length   <- sum(row_sub$N_length)
  N_constant <- sum(row_sub$N_constant)
  
  dat_row <- cbind(dat_row, N_length, N_constant)
  new_dat <- rbind(new_dat, dat_row)
}

write.table(x = new_dat, file = paste0(dirpath, '/ichthyop_output_mortality.csv'), sep = ';', row.names = F)
