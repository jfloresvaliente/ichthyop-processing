#=============================================================================#
# Name   : get_subset_recruited
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
dirpath  <- 'E:/ICHTHYOP/10kmparent/FISICA/out/results/'
days     <- 60

#========== Do not change anything after here ==========#
df_ini <- NULL # Data frame for initial positions of recruited particles
df_end <- NULL # Data frame for final positions of recruited particles

for(j in 1:12){
  Rdata <- paste0(dirpath,'trajectoriesM',j,'.Rdata')
  load(Rdata)
  print(Rdata)
  dat <- trajectories; rm(trajectories)
  
  index <- subset(dat, dat$Timer == (days+1) & dat$IfRecruited == 1)$Drifter
  
  recruited <- subset(dat, dat$Drifter %in% index)
  recruited <- matrix(data = recruited$IfRecruited, ncol = (days+1), nrow = dim(recruited)[1]/(days+1), byrow = T)
  recruited_day    <- apply(recruited, 1, which.max)
  
  for(u in seq_along(recruited_day)){
    inicol <- recruited_day[u]
    
    if(inicol == days+1) next()
    recruited[u, (inicol+1) : dim(recruited)[2]] <- NA
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
  
  names_col <- c('Drifter', 'Lon', 'Lat', 'Depth', 'Zone_name', 'ReleaseDepth', 'ReleaseBathy', 'AgeRecruited', 'Month')
  names_ind <- NULL
  for(n in seq_along(names_col)){
    names_ind <- c(names_ind, which(names(dat) == names_col[n]))
  }
  dat <- dat[, names_ind]
  
  df_end <- rbind(df_end, dat)
  
  recruited_drifters <- dat$Drifter
  load(Rdata)
  dat2 <- trajectories; rm(trajectories)
  dat2 <- subset(dat2, dat2$Drifter %in% recruited_drifters & dat2$Timer == 1)
  
  names_col <- c('Drifter', 'Lon', 'Lat', 'Depth')
  names_ind <- NULL
  for(n in seq_along(names_col)){
    names_ind <- c(names_ind, which(names(dat2) == names_col[n]))
  }
  
  dat2 <- cbind(dat2[,names_ind], dat$Zone_name, dat$ReleaseDepth, dat$ReleaseBathy, dat$AgeRecruited, dat$Month)
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