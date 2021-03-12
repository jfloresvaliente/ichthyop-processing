#=============================================================================#
# Name   : get_curves_age_at_recruitment_byReleaseDepthYearMean
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Gets the percentage of particles at age at recruitment by Release Depth.
# URL    : 
#=============================================================================#
dirpath   <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB/k_x1.6_90days/out/results/'
days      <- 91 # Step time
depth_lev <- c('0-15', '15-30', '30-45')

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
if(!file.exists(paste0(dirpath, 'recruited_age_percentageReleaseDepthYearMean.Rdata'))){
  
  age_count_percentageYearMean <- NULL
  for(j in 1:length(depth_lev)){
    depth <- depth_lev[j]
    print(paste('Getting', depth, 'data...'))
    
    particles_all <- 0
    recruited_day_all <- NULL
    for(i in 1:12){
      
      Rfile <- paste0(dirpath, 'trajectoriesM', i, '.Rdata')
      print(Rfile)
      load(Rfile)
      
      dat <- trajectories; rm(trajectories)
      dat <- subset(dat, dat$ReleaseDepth == depth)
      
      particles <- dim(subset(dat, dat$Timer == 1))[1]
      particles_all <- particles_all + particles
      
      dat  <- subset(dat, dat$Timer <= (days))
      index <- subset(dat, dat$Timer == (days) & dat$IfRecruited == 1)$Drifter
      recruited <- subset(dat, dat$Drifter %in% index)
      recruited <- matrix(data = recruited$IfRecruited, ncol = (days), nrow = dim(recruited)[1]/(days), byrow = T)
      recruited_day <- apply(recruited, 1, which.max)
      recruited_day <- recruited_day - 1 # Hay que restar 1, para obtener la edad real
      recruited_day <- hist(recruited_day,0:(days),plot = FALSE)$counts
      recruited_day_all <- cbind(recruited_day_all, recruited_day)
    }
    recruited_day_all <- apply(X = recruited_day_all, MARGIN = 1, FUN = sum)
    recruited_day_all <- (recruited_day_all*100)/particles_all
    
    age_count_percentageYearMean <- rbind(age_count_percentageYearMean, recruited_day_all)
  }
  recruited_age_percentageReleaseDepthYearMean <- age_count_percentageYearMean
  save(recruited_age_percentageReleaseDepthYearMean, file = paste0(dirpath, 'recruited_age_percentageReleaseDepthYearMean.Rdata'))
}else{
  print('An [R.data] file already exists ...')
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#