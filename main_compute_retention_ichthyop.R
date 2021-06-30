#=============================================================================#
# Name   : main_compute_retention_ichthyop
# Author : C. Lett; modified by Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Compute recruitment from ICHTHYOP outputs (.nc)
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')
source('source/compute_retention_ichthyop.R')
# source('ichthyop_functions.R')

dirpath  <- 'D:/ICHTHYOP/10kmparent/DEBf1/k_x0/out/'
new_path <- 'D:/ICHTHYOP/10kmparent/DEBf1/cfg/'
recruit_zone    <- as.matrix(read.table('C:/Users/jflores/Documents/ICHTHYOP/ichthyop_recruitment_polygon.txt', header = T, sep = ''))
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
nc              <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[1])
cfgnc           <- gsub(pattern = '\\\\', replacement = '/', x = ncatt_get(nc = nc, 0 , 'xml_file')$value)
old_path        <- substr(x = cfgnc , start = 1 , stop = str_locate(string = cfgnc, pattern = 'cfg')[2])
firstdrifter    <- 1
lastdrifter     <- as.numeric(ncatt_get(nc , 0 , 'release.zone.number_particles')$value)
# computeattime   <- 31 # length(ncvar_get(nc, 'time'))
nbreleasezones  <- ncatt_get(nc , 0 , 'nb_zones')$value -1
recruitmentzone <- 1
dates           <- read.table(paste0(new_path, 'date_scrum_time_ichthyop.csv'), header = T, sep = ';')
nc_close(nc)

# dat <- compute_retention_ichthyop(dirpath          = dirpath,
#                                     firstdrifter     = firstdrifter
#                                     ,lastdrifter     = lastdrifter
#                                     ,computeattime   = computeattime
#                                     ,nbreleasezones  = nbreleasezones
#                                     ,recruitmentzone = recruitmentzone
#                                     ,old_path        = old_path
#                                     ,new_path        = new_path
#                                     ,dates           = dates
#                                     ,recruit_zone    = recruit_zone
# )
# 
# for(i in 1:9) dat$Zone_name[grep(pattern = paste0('zone', i), x = dat$Zone_name)] <- paste0('zone', i)
# dir.create(path = paste0(dirpath, 'results'), showWarnings = F)
# write.table(x = dat, file = paste0(dirpath, '/results/ichthyop_output_retention.csv'), sep = ';', row.names = F)

days   <- seq(from = 11, to = 91, by = 10) # length(ncvar_get(nc, 'time'))
for(j in 1:length(days)){
  computeattime <- days[j]
  dat <- compute_retention_ichthyop(dirpath          = dirpath,
                                    firstdrifter     = firstdrifter
                                    ,lastdrifter     = lastdrifter
                                    ,computeattime   = computeattime
                                    ,nbreleasezones  = nbreleasezones
                                    ,recruitmentzone = recruitmentzone
                                    ,old_path        = old_path
                                    ,new_path        = new_path
                                    ,dates           = dates
                                    ,recruit_zone    = recruit_zone
  )
  
  for(i in 1:9) dat$Zone_name[grep(pattern = paste0('zone', i), x = dat$Zone_name)] <- paste0('zone', i)
  dir.create(path = paste0(dirpath, 'results'), showWarnings = F)
  write.table(x = dat, file = paste0(dirpath, '/results/ichthyop_output_retention_',days[j],'days.csv'), sep = ';', row.names = F)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#