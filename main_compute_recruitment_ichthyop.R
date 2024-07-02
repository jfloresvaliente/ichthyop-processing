#=============================================================================#
# Name   : main_compute_recruitment_ichthyop
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Compute recruitment from ICHTHYOP outputs (.nc)
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')
source('ichthyop_functions.R')

dirpath  <- 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj_shape_pecq/case1_kx0.4/'
new_path <- 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj_shape_pecq/cfg/'

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
nc              <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[1])
cfgnc           <- gsub(pattern = '////', replacement = '/', x = ncatt_get(nc = nc, 0 , 'xml_file')$value)
old_path        <- substr(x = cfgnc , start = 1 , stop = str_locate(string = cfgnc, pattern = 'cfg')[2])
firstdrifter    <- 1
lastdrifter     <- as.numeric(ncatt_get(nc , 0 , 'release.zone.number_particles')$value)
computeattime   <- length(ncvar_get(nc, 'time'))
nbreleasezones  <- ncatt_get(nc , 0 , 'nb_zones')$value - 1
recruitmentzone <- 1
dates           <- read.table(paste0(new_path, 'date_scrum_time_ichthyop.csv'), header = T, sep = ';')

nc_close(nc)

dat <- compute_recruitment_ichthyop(dirpath            = dirpath,
                                    firstdrifter       = firstdrifter
                                    ,lastdrifter       = lastdrifter
                                    ,computeattime     = computeattime
                                    ,nbreleasezones    = nbreleasezones
                                    ,recruitmentzone   = recruitmentzone
                                    ,old_path          = old_path
                                    ,new_path          = new_path
                                    ,dates             = dates
)

for(i in 1:9) dat$Zone_name[grep(pattern = paste0('zone', i), x = dat$Zone_name)] <- paste0('zone', i)

dir.create(path = paste0(dirpath, '/results'), showWarnings = F)
write.table(x = dat, file = paste0(dirpath, '/results/ichthyop_output.csv'), sep = ';', row.names = F)

#===== Para calcular el reclutamiento dia por dia =====#
# days   <- seq(from = 1, to = 91, by = 1) # length(ncvar_get(nc, 'time'))
# for(j in 1:length(days)){
#   computeattime <- days[j]
# dat <- compute_recruitment_ichthyop(dirpath          = dirpath,
#                                     firstdrifter     = firstdrifter
#                                     ,lastdrifter     = lastdrifter
#                                     ,computeattime   = computeattime
#                                     ,nbreleasezones  = nbreleasezones
#                                     ,recruitmentzone = recruitmentzone
#                                     ,old_path        = old_path
#                                     ,new_path        = new_path
#                                     ,dates           = dates
# )
# 
# for(i in 1:9) dat$Zone_name[grep(pattern = paste0('zone', i), x = dat$Zone_name)] <- paste0('zone', i)
# dir.create(path = paste0(dirpath, 'results'), showWarnings = F)
# write.table(x = dat, file = paste0(dirpath, '/results/ichthyop_output',days[j],'days.csv'), sep = ';', row.names = F)
# }
#=============================================================================#
# END OF PROGRAM
#=============================================================================#