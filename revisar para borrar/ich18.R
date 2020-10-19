#=============================================================================#
# Name   : plot_trajectories_particles
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Hovmuller diagram of the retention rates
# URL    : 
#=============================================================================#
library(fields)
# source('source/plot_traj_ggmap_depth.R')

dirpath   <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/lati220/results_fisica/'
new_path  <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/cfg/'
xy              <- read.table(paste0(new_path, 'peru_drifters.csv'), sep = ';')

#------------- Do not change anything after here -------------#
#Internal function to calculate error bars
error_bar <- function(x){
  # x = vector or matrix with data to evaluate
  
  if(!is.null(a)){
    n  <- length(x)
    m  <- mean(x, na.rm = T)
    tt <- -qt(p = a/2, df = n-1)
    ee <- sd(x)/sqrt(n)  # standard error
    e  <- tt*ee          # error range
    d  <- e/m            # relative error, says that the confidence interval is a percentage of the value
    li <- m-e            # lower limit
    ls <- m+e            # upper limit
    stat <- c(m, li, ls)
  }else{
    n  <- length(x)
    m  <- mean(x, na.rm = T)
    tt <- -qt(p = a/2, df = n-1)
    ee <- sd(x)/sqrt(n)  # standard error
    li <- m-ee           # lower limit
    ls <- m+ee           # upper limit
    stat <- c(m, li, ls)
  }
  return(stat)
}

x11()
par(mfrow = c(3,4), mar = c(4,4,1,1))
for(j in 1:12){
  # Read data output from Ichthyop simulation
  load(paste0(dirpath, 'trajectoriesM',j,'.Rdata'))
  dat <- trajectories; rm(trajectories)
  # dat <- subset(dat, dat$IfRecruited == 1)
  
  lasttime  <- subset(dat, dat$Timer == 31)
  rec1 <- subset(lasttime, lasttime$IfRecruited == 1)
  rec1 <- subset(dat, dat$Drifter %in% rec1$Drifter)
  
  rec2 <- subset(lasttime, lasttime$IfRecruited == 0)
  rec2 <- subset(dat, dat$Drifter %in% rec2$Drifter)
  
  a = 0.05
  rec1_error <- NULL
  rec2_error <- NULL
  for(i in 1:31){
    err1 <- subset(rec1, rec1$Timer == i)
    rec1_error <- rbind(rec1_error, error_bar(err1$MESO))
    
    err2 <- subset(rec2, rec2$Timer == i)
    rec2_error <- rbind(rec2_error, error_bar(err2$MESO))
  }
  
  plot(1:31, type = 'n', ylim = c(0,3), xlab = '', ylab = '')
  lines(1:31, rec1_error[,1])
  lines(1:31, rec2_error[,1], col = 'red')
  if(j == 1 | j == 5 | j == 9)             mtext(side = 2, text = 'Mesozoo (umol C L-1)', line = 1)
  if(j == 9 | j == 10 | j == 11 | j == 12) mtext(side = 1, text = 'Days', line = 1)
  legend('topleft', legend = c('Recruited', 'Non-Recruited'), text.col = c('black','red'), bty = 'n')
}






# arrows(x0 = 1:31, y0 = rec1_error[,2], x1 = 1:31, y1 = rec1_error[,3],angle = 90, code = 3, length = 0.05)
# arrows(x0 = 1:31, y0 = rec2_error[,2], x1 = 1:31, y1 = rec2_error[,3],angle = 90, code = 3, length = 0.05, col = 'red')

#

# maxlength <- which.max(lasttime$length)
# maxlength <- subset(dat, dat$Drifter == maxlength)
# 
# minlength <- which.min(lasttime$length)
# minlength <- subset(dat, dat$Drifter == minlength)

# latdrif <- subset(xy, xy$V3 %in% c(2))
# 
# drifters <- subset(dat, dat$Lon %in% latdrif$V1 & dat$Lat %in% latdrif$V2)
# 
# dat <- subset(dat, dat$Drifter %in% drifters$Drifter & dat$IfRecruited == 1)
# 
# x11();plot_traj_ggmap_depth(df = dat, xlim = c(-85,-70))
