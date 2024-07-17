#=============================================================================#
# Name   : main_get_data_atRecruitmentAgeDirecto
# Author : C. Lett; modified by Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Compute recruitment from ICHTHYOP outputs (.nc)
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')
source('ichthyop_functions.R')

dirpath  <- 'E:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj_shape_pecq/case1/'
new_path <- 'E:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj_shape_pecq/cfg/'
variname <- c('length','MESO','temp')
# variname <- NULL

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
nc              <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[1])
cfgnc           <- gsub(pattern = '\\\\', replacement = '/', x = ncatt_get(nc = nc, 0 , 'xml_file')$value)
old_path        <- substr(x = cfgnc , start = 1 , stop = str_locate(string = cfgnc, pattern = 'cfg')[2])
firstdrifter    <- 1
lastdrifter     <- as.numeric(ncatt_get(nc , 0 , 'release.zone.number_particles')$value)
firsttime       <- 1
lasttime        <- length(ncvar_get(nc, 'time'))
recruitmentzone <- 1
dates           <- read.table(paste0(new_path, 'date_scrum_time_ichthyop.csv'), header = T, sep = ';')
N0              <- 1 # Initial value of the particle affected by mortality
freq_record     <- 1 # Record frequency in Ichthyop
nc_close(nc)

df <- get_data_atRecruitmentAgeDirecto(
  dirpath          = dirpath
  ,firstdrifter    = firstdrifter
  ,lastdrifter     = lastdrifter
  ,firsttime       = firsttime
  ,lasttime        = lasttime
  ,recruitmentzone = recruitmentzone
  ,old_path        = old_path
  ,new_path        = new_path
  ,dates           = dates
  ,variname        = variname
  ,N0              = 1
  ,freq_record     = 1
)

dir.create(path = paste0(dirpath, 'results'), showWarnings = F)
Rdata  <- paste0(dirpath, '/results/data_atRecruitmentAge.Rdata')
print(paste0('saving .....', Rdata))
save(df, file = Rdata)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#