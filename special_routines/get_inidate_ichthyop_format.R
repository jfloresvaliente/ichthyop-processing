#=============================================================================#
# Name   : get_inidate_ichthyop_format
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Make the initial date for ichthyop simu in XML format
# URL    : 
#=============================================================================#
dirpath <- 'D:/ROMS_SILUMATIONS/rsodi1/'
dat <- read.table(paste0(dirpath,'date_scrum_time_ichthyop.csv'), header = T, sep = ';')

dat <- subset(dat, dat$Y %in% c(1980:2000) & dat$t_x %in% c(1,3,5))

date_sim <- NULL
for(i in 1:dim(dat)[1]){
  dat2 <- dat[i,]
  
  year <- dat2$sim_year
  
  month <- dat2$sim_month
  if(month < 10) month <- paste0(0,month)
  
  day <- dat2$sim_day
  if(day < 10) day <- paste0(0,day)
  
  date_sim <- c(date_sim, paste('<value>year', year, 'month', month, 'day', day, 'at 13:00</value>'))
}
write.table(x = date_sim, file = paste0(dirpath,'date_init_ichthyop_format.csv'), row.names = F, col.names = F)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#