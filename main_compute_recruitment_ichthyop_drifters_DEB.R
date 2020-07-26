#=============================================================================#
# Name   : main_compute_recruitment_ichthyop_drifters_DEB
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Compute recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
source('source/source_libraries_functions.R')

dirpath   <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/lati2-20/'
new_path  <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/cfg/'
ymax      <- 20

#---- Do not change anythig after here ----#
nc              <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[1])
firstdrifter    <- 1
lastdrifter     <- 50498 #dim(read.table(paste0(new_path, 'peru_drifters.txt')))[1]
firsttime       <- 1
lasttime        <- length(ncvar_get(nc, 'time'))
recruitmentzone <- 1
dates           <- read.table(paste0(new_path, 'date_scrum_time_ichthyop.csv'), header = T, sep = ';')
xy              <- read.table(paste0(new_path, 'peru_drifters.csv'), sep = ';')
length_min      <- 20
depth_min       <- 50
polyg <- read.table(paste0(new_path, 'ichthyop_recruitment_polygon.txt'))

dat <- compute_recruitment_ichthyop_drifters_DEB(dirpath     = dirpath,
                                             firstdrifter    = firstdrifter,
                                             lastdrifter     = lastdrifter,
                                             firsttime       = firsttime,
                                             lasttime        = lasttime,
                                             recruitmentzone = recruitmentzone,
                                             dates           = dates,
                                             xy              = xy,
                                             length_min      = length_min,
                                             depth_min       = depth_min,
                                             polyg           = polyg
)

dir.create(path = paste0(dirpath, 'results'), showWarnings = F)
write.table(x = dat, file = paste0(dirpath, '/results/ichthyop_output.csv'), sep = ';', row.names = F)



# x11();par(mfrow = c(1,3))
# # Por year
# year <- levels(factor(dat$Year))
# yearper <- NULL
# for(i in 1:length(year)){
#   df <- subset(dat, dat$Year == year[i])
#   rec <- sum(df$IfRecruited)
#   yearper <- c(yearper, (rec*100)/dim(df)[1])
# }
# year <- cbind(year, yearper)
# 
# # Por mes
# day <- levels(factor(dat$Day))
# dayper <- NULL
# for(i in 1:length(day)){
#   df <- subset(dat, dat$Day == day[i])
#   rec <- sum(df$IfRecruited)
#   dayper <- c(dayper, (rec*100)/dim(df)[1])
# }
# day <- cbind(as.numeric(day), as.numeric(dayper))
# barplot(day[,2], names.arg = day[,1], xlab = 'Release Month', ylab = 'Larval Retention (%)', ylim = c(0,ymax))
# 
# # Por depth
# depth <- levels(factor(dat$Depth))
# depthper <- NULL
# for(i in 1:length(depth)){
#   df <- subset(dat, dat$Depth == depth[i])
#   rec <- sum(df$IfRecruited)
#   depthper <- c(depthper, (rec*100)/dim(df)[1])
# }
# depth <- cbind(as.numeric(depth), as.numeric(depthper))
# barplot(depth[,2], names.arg = depth[,1], xlab = 'Release Depth (m)', ylab = 'Larval Retention (%)', ylim = c(0,ymax))
# 
# # Por latitud
# lati <- levels(factor(dat$ReleaseArea))
# latiper <- NULL
# for(i in 1:length(lati)){
#   df <- subset(dat, dat$ReleaseArea == lati[i])
#   rec <- sum(df$IfRecruited)
#   latiper <- c(latiper, (rec*100)/dim(df)[1])
# }
# lati <- cbind(as.numeric(lati), as.numeric(latiper))
# barplot(lati[,2], names.arg = lati[,1], xlab = 'Latitude of Release (º)', ylab = 'Larval Retention (%)', ylim = c(0,ymax))
# 
# # Por pixel
# pixel <- levels(factor(dat$PixelCoast))
# pixelper <- NULL
# for(i in 1:length(pixel)){
#   df <- subset(dat, dat$PixelCoast == pixel[i])
#   rec <- sum(df$IfRecruited)
#   pixelper <- c(pixelper, (rec*100)/dim(df)[1])
# }
# pixel <- cbind(pixel, pixelper)
# 
# km <- as.numeric(pixel[,1])*10
# re <- as.numeric(pixel[,2])
# 
# x11(); plot(km, re, ylim = c(0,40)); abline(h = 0, col = 'grey90')

#=============================================================================#
# END OF PROGRAM
#=============================================================================#