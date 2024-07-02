#=============================================================================#
# Name   : get_data_atRecruitmentAge
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(Hmisc)
dirpath       <- 'E:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj_shape_pecq/case1_kx1.6/'
computeattime <- 91
freq_record   <- 1 # Record frequency in Ichthyop

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
df <- NULL 
for(i in 1:12){

  Rdata <- paste0(dirpath,'/results/trajectoriesM',i,'.Rdata')
  print(Rdata)
  load(Rdata)
  dat <- trajectories; rm(trajectories)
  dat$Age <- (dat$Timer-1)/freq_record

  # Get recruited particles
  recruited_drifter <- subset(dat, dat$Timer == computeattime & dat$IfRecruited == 1)$Drifter
  recruited         <- subset(dat, dat$Drifter %in% recruited_drifter)
  recruitment_timer_matrix <- matrix(data = recruited$IfRecruited, ncol = computeattime, nrow = dim(recruited)[1]/computeattime, byrow = T)
  recruitment_timer <- apply(recruitment_timer_matrix, 1, which.max)
  
  for(j in seq_along(recruitment_timer)){
    inicol <- recruitment_timer[j]
    
    if(inicol == computeattime) next()
    recruitment_timer_matrix[j, (inicol+1) : computeattime] <- NA
  }
  recruitment_timer_matrix <- as.vector(t(recruitment_timer_matrix))
  
  Lon_ini <- subset(recruited, recruited$Timer == 1)$Lon
  Lat_ini <- subset(recruited, recruited$Timer == 1)$Lat
  
  recruited <- recruited[which(recruitment_timer_matrix == 1),]
  recruited$Lon_ini <- Lon_ini
  recruited$Lat_ini <- Lat_ini

  # Get non-recruited particles
  non_recruited <- subset(dat, dat$Drifter %nin% recruited_drifter)
  
  non_recruited_ini <- subset(non_recruited, non_recruited$Timer == 1)
  Lon_ini <- subset(non_recruited_ini, non_recruited_ini$Timer == 1)$Lon
  Lat_ini <- subset(non_recruited_ini, non_recruited_ini$Timer == 1)$Lat
  
  non_recruited_fin <- subset(non_recruited, non_recruited$Timer == computeattime)
  non_recruited_fin$Lon_ini <- Lon_ini
  non_recruited_fin$Lat_ini <- Lat_ini

  particles <- rbind(recruited, non_recruited_fin)

df <- rbind(df, particles)
}

rm_col <- c('Drifter','Timer','Depth','length','MESO','temp')
rm_col_ind <- NULL
for(i in 1:length(rm_col)){
  rm_ind <- which(names(df) == rm_col[i])
  rm_col_ind <- c(rm_col_ind, rm_ind)
}

df <- df[,-c(rm_col_ind)]

names_col <- c('Lon_end', 'Lat_end', 'IfRecruited', 'Mortality', 'ReleaseArea',
               'Year', 'Month', 't_x','Zone_name', 'ReleaseDepth',
               'ReleaseBathy', 'N_length', 'N_constant', 'Age', 'Lon_ini', 'Lat_ini')
colnames(df) <- names_col

Rdata  <- paste0(dirpath, '/results/data_atRecruitmentAge.Rdata')
print(paste0('saving .....', Rdata))
save(df, file = Rdata)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#