#=============================================================================#
# Name   : main_get_trajectories_drifters_DEB
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Get trajectories from ICHTHYOP simulations
# URL    : 
#=============================================================================#
source('source/source_libraries_functions.R')

dirpath   <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/lati220/'
new_path  <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/cfg/'

#---- Do not change anythig after here ----#
ncfile          <- list.files(path = dirpath, pattern = '.nc', full.names = T)[1]
nc              <- nc_open(ncfile)
cfgnc           <- gsub(pattern = '\\\\', replacement = '/', x = ncatt_get(nc = nc, 0 , 'xml_file')$value)
old_path        <- substr(x = cfgnc , start = 1 , stop = str_locate(string = cfgnc, pattern = 'cfg')[2])
firstdrifter    <- 1
lastdrifter     <- 50498 #dim(read.table(paste0(new_path, 'peru_drifters.txt')))[1]
firsttime       <- 1
lasttime        <- length(ncvar_get(nc, 'time'))
recruitmentzone <- 1
variname        <- c('stage','E','length','MESO','temp','O2')
length_min      <- 20

# The paths of all .nc ichthyop files will be extracted month by month and combined into a single .RData file
dat <- read.table(paste0(dirpath, '/results/ichthyop_output.csv'), header = T, sep = ';')

for(i in 1:12){
  month <- subset(dat, dat$Day == i)
  month <- levels(factor(month$Name_file))
  
  trajectories <- NULL
  for(j in 1:length(month)){
    ncfile <- paste0(dirpath, month[j], '.nc')
    print(ncfile)
    trajs <- get_trajectories_drifters_DEB(ncfile  = ncfile
                                       ,firstdrifter    = firstdrifter
                                       ,lastdrifter     = lastdrifter
                                       ,firsttime       = firsttime
                                       ,lasttime        = lasttime
                                       ,recruitmentzone = recruitmentzone
                                       ,old_path        = old_path
                                       ,new_path        = new_path
                                       ,variname        = variname
                                       ,length_min      = length_min)
    trajectories <- rbind(trajectories, trajs)
  }
  trajectories$Drifter <- rep(seq(1, lastdrifter*length(month)), each = lasttime)
  # Saving on object in RData format
  RData <- paste0(dirpath, '/results/', 'trajectoriesM', i, '.Rdata')
  print(paste0('saving .....', RData))
  save(trajectories, file = RData)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#