#=============================================================================#
# Name   : get_curves_age_at_recruitment_byReleaseDepth
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Gets the percentage of particles at age at recruitment by Release Depth.
# URL    : 
#=============================================================================#
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEBf1/k_x0_90days/out/results/'
days    <- 91 # Step time
depth   <- '30-45'

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
if(!file.exists(paste0(dirpath, 'recruited_age_percentage', depth, '.Rdata'))){
  age_count_percentage <- NULL
  
  for(i in 1:12){
    
    Rfile <- paste0(dirpath, 'trajectoriesM', i, '.Rdata')
    print(Rfile)
    load(Rfile)
    
    dat <- trajectories; rm(trajectories)
    dat <- subset(dat, dat$ReleaseDepth == depth)
    
    particles <- dim(subset(dat, dat$Timer == 1))[1]
    
    dat  <- subset(dat, dat$Timer <= (days))
    index <- subset(dat, dat$Timer == (days) & dat$IfRecruited == 1)$Drifter
    recruited <- subset(dat, dat$Drifter %in% index)
    recruited <- matrix(data = recruited$IfRecruited, ncol = (days), nrow = dim(recruited)[1]/(days), byrow = T)
    recruited_day <- apply(recruited, 1, which.max)
    recruited_day <- recruited_day - 1 # Hay que restar 1, para obtener la edad real
    recruited_day <- hist(recruited_day,0:(days),plot = FALSE)$counts
    recruited_day_percentage <- (recruited_day*100)/particles
    
    age_count_percentage <- cbind(age_count_percentage, recruited_day_percentage)
  }
  recruited_age_percentage <- age_count_percentage
  save(recruited_age_percentage, file = paste0(dirpath, 'recruited_age_percentage', depth, '.Rdata'))
}else{
  print('An [R.data] file already exists ...')
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#