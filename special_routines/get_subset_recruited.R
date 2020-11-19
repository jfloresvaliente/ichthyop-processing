#=============================================================================#
# Name   : get_subset_recruited
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
dirpath       <- 'C:/Users/jflores/Desktop/ich_deb/DEB/'
computeattime <- 61

#========== Do not change anything after here ==========#
df_ini <- NULL # Data frame for initial positions of recruited particles
df_end <- NULL # Data frame for final positions of recruited particles

for(j in 1:12){
  Rdata <- paste0(dirpath,'trajectoriesM',j,'.Rdata')
  load(Rdata)
  print(Rdata)
  dat <- trajectories; rm(trajectories)
  
  # Mortalidad basada en longitud
  dat$M_length <- 0.189 * exp(-(dat$length)/10 / 2.468) # Brochier et al 2018, dividir entre 10, porque se aplica en cm
  dat$length <- dat$length/10  # dividir entre 10, porque se aplica en cm
  dat2 <- dat
  
  # Calcular el valor de la particula en funcion de la mortalidad M_length basada en longitud
  morta <- matrix(data = dat$M_length, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  sup_indi <- 1
  N_value <- matrix(data = NA, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  N_value[,1] <- sup_indi
  
  for(s in 2:computeattime){
    N_value[,s] <- N_value[,s-1]*(1-morta[,s-1])
  }
  dat$N_value <- as.vector(t(N_value))
  
  # Calcular el valor de la particula en funcion de la mortalidad constante 0.1
  morta <- matrix(data = 0.1, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  sup_indi <- sup_indi
  N_value_constant <- matrix(data = NA, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  N_value_constant[,1] <- sup_indi
  
  for(s in 2:computeattime){
    N_value_constant[,s] <- N_value_constant[,s-1]*(1-morta[,s-1])
  }
  dat$N_value_constant <- as.vector(t(N_value_constant))
  
  # Obtener el dia de reclutamiento
  index <- subset(dat, dat$Timer == computeattime & dat$IfRecruited == 1)$Drifter
  recruited <- subset(dat, dat$Drifter %in% index)
  recruited <- matrix(data = recruited$IfRecruited, ncol = computeattime, nrow = dim(recruited)[1]/computeattime, byrow = T)
  recruited_day <- apply(recruited, 1, which.max)
  
  for(u in seq_along(recruited_day)){
    inicol <- recruited_day[u]
    
    if(inicol == computeattime) next()
    recruited[u, (inicol+1) : computeattime] <- NA
  }
  
  recruited <- as.vector(t(recruited))
  
  dat <- subset(dat, dat$Drifter %in% index)
  dat <- cbind(dat, recruited)
  
  dat <- subset(dat, dat$recruited == 1)
  dat$AgeRecruited <- recruited_day-1
  dat$Month <- j
  
  for(i in 1:9){
    zone_name <- paste0('zone', i)
    a <- grep(pattern = zone_name, x = dat$Zone_name)
    dat$Zone_name[a] <- zone_name
  }
  
  # Get an aleatory sample of 1000 particles
  # set.seed(10^2)
  # dat <- dat[sample(nrow(dat), 1000), ]
  
  names_col <- c('Drifter', 'Lon', 'Lat', 'Depth', 'Zone_name', 'ReleaseDepth', 'ReleaseBathy', 'length', 'AgeRecruited', 'Month', 'M_length', 'N_value', 'N_value_constant')
  names_ind <- NULL
  for(n in seq_along(names_col)){
    names_ind <- c(names_ind, which(names(dat) == names_col[n]))
  }
  dat <- dat[, names_ind]
  
  df_end <- rbind(df_end, dat)
  
  # Obtener las posiciones iniciales
  recruited_drifters <- dat$Drifter
  dat2 <- subset(dat2, dat2$Drifter %in% recruited_drifters & dat2$Timer == 1)
  
  names_col <- c('Drifter', 'Lon', 'Lat', 'Depth')
  names_ind <- NULL
  for(n in seq_along(names_col)){
    names_ind <- c(names_ind, which(names(dat2) == names_col[n]))
  }
  
  dat2 <- cbind(dat2[,names_ind], dat$Zone_name, dat$ReleaseDepth, dat$ReleaseBathy, dat$length, dat$AgeRecruited, dat$Month, dat$M_length, dat$N_value, dat$N_value_constant)
  colnames(dat2) <- names(dat)
  df_ini <- rbind(df_ini, dat2)
  
  rm(dat, dat2)
}

df_ini <- df_ini[,-c(1)]; df_ini$Month <- as.factor(df_ini$Month)
RData  <- paste0(dirpath, 'recruited_subset_inipos.Rdata')
print(paste0('saving .....', RData))
save(df_ini, file = RData)

df_end <- df_end[,-c(1)]; df_end$Month <- as.factor(df_end$Month)
RData  <- paste0(dirpath, 'recruited_subset_endpos.Rdata')
print(paste0('saving .....', RData))
save(df_end, file = RData)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#