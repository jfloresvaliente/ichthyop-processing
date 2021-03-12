#=============================================================================#
# Name   : get_data_atRecruitmentAge
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
dirpath       <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/10kmparent/DEB/k_x1.6/out/results/'
computeattime <- 61
N0            <- 1 # Initial value of the particle affected by mortality

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
df <- NULL 
for(j in 1:12){
  Rdata <- paste0(dirpath,'trajectoriesM',j,'.Rdata')
  load(Rdata)
  print(Rdata)
  dat <- trajectories; rm(trajectories)
  dat$length <- dat$length/10 #convert from mm to cm

  # Get length-dependent mortality matrix (M_length)
  # Get value of each particle affected by length-dependent mortality (N_length)
  M_length <- 0.189 * exp( -(dat$length) / 2.468) # Brochier et al 2018, dividir entre 10, porque se aplica en cm
  M_length <- matrix(data = M_length, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  
  N_length <- matrix(data = NA, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  N_length[,1] <- N0
  
  for(i in 2:computeattime){
    N_length[,i] <- N_length[,i-1] * (1 - M_length[,i-1])
  }
  dat$N_length <- as.vector(t(N_length))
  
  # Get constant mortality matrix (M_constant)
  # Get value of each particle affected by constant mortality (N_constant)
  M_constant <- matrix(data = 0.1, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  
  N_constant <- matrix(data = NA, ncol = computeattime, nrow = dim(dat)[1]/computeattime, byrow = T)
  N_constant[,1] <- N0
  
  for(i in 2:computeattime){
    N_constant[,i] <- N_constant[,i-1] * (1 - M_constant[,i-1])
  }
  dat$N_constant <- as.vector(t(N_constant))

  # Get age at recruitment
  drifter_index <- subset(dat, dat$Timer == computeattime & dat$IfRecruited == 1)$Drifter
  recruitment_age <- subset(dat, dat$Drifter %in% drifter_index)
  recruitment_age <- matrix(data = recruitment_age$IfRecruited, ncol = computeattime, nrow = dim(recruitment_age)[1]/computeattime, byrow = T)
  age_at_recruitment <- apply(recruitment_age, 1, which.max)
  
  for(i in seq_along(age_at_recruitment)){
    inicol <- age_at_recruitment[i]
    
    if(inicol == computeattime) next()
    recruitment_age[i, (inicol+1) : computeattime] <- NA
  }
  recruitment_age <- as.vector(t(recruitment_age))
  
  # Get INITIAL (lon, lat) positions of retained particles
  dat <- subset(dat, dat$Drifter %in% drifter_index)
  Lon_ini <- subset(dat$Lon, dat$Timer == 1)
  Lat_ini <- subset(dat$Lat, dat$Timer == 1)
  
  # Add age at recruitment to a new subset of recruited particles
  dat <- cbind(dat, recruitment_age)
  dat <- subset(dat, dat$recruitment_age == 1)
  dat$AgeRecruited <- age_at_recruitment-1
  dat$Month <- j
  
  # Latitudinal zone identifier (9 zones)
  for(i in 1:9){
    zone_name <- paste0('zone', i)
    a <- grep(pattern = zone_name, x = dat$Zone_name)
    dat$Zone_name[a] <- zone_name
  }

  # Get and save the FINAL position information of age at recruitment
  names_col <- c('Lon','Lat','Depth','Year','Zone_name','ReleaseDepth','ReleaseBathy','t_x','length','AgeRecruited','Month','N_length','N_constant')
  names_ind <- NULL
  for(i in seq_along(names_col)) names_ind <- c(names_ind, which(names(dat) == names_col[i]))
  dat <- dat[, names_ind]
  dat <- cbind(Lon_ini, Lat_ini, dat)
  
  colnames(dat) <- c('Lon_ini', 'Lat_ini', 'Lon_end', 'Lat_end', names_col[3:length(names_col)])
  df <- rbind(df, dat)
}
rownames(df) <- 1:dim(df)[1]

RData  <- paste0(dirpath, 'data_atRecruitmentAge.Rdata')
print(paste0('saving .....', RData))
save(df, file = RData)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#