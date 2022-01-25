#=============================================================================#
# Name   : add_mortality_toRData
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
# dirpath       <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC1/out19C/results/'
computeattime <- 91
N0            <- 1 # Initial value of the particle affected by mortality

for(i in 1:12){
  RData <- paste0(dirpath,'trajectoriesM',i,'.RData')
  load(RData)
  dat <- trajectories; rm(trajectories)
  talla <- dat$length/10 #convert from mm to cm
  
  # Get length-dependent mortality matrix (M_length)
  M_length <- 0.189 * exp( -(talla) / 2.468) # Brochier et al 2018, dividir entre 10, porque se aplica en cm
  M_length <- matrix(data = M_length, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  
  # Get value of each particle affected by length-dependent mortality (N_length)
  N_length <- matrix(data = NA, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  N_length[,1] <- N0
  
  for(i in 2:computeattime){
    N_length[,i] <- N_length[,i-1] * (1 - M_length[,i-1])
  }
  dat$N_length <- as.vector(t(N_length))
  
  # Get constant mortality matrix (M_constant)
  M_constant <- matrix(data = 0.1, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  
  # Get value of each particle affected by constant mortality (N_constant)
  N_constant <- matrix(data = NA, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  N_constant[,1] <- N0
  
  for(i in 2:computeattime){
    N_constant[,i] <- N_constant[,i-1] * (1 - M_constant[,i-1])
  }
  dat$N_constant <- as.vector(t(N_constant))
  
  trajectories <- dat
  print(paste0('re-saving .....', RData))
  save(trajectories, file = RData)
}
