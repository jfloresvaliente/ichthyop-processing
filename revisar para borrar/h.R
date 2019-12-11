# dirpath       <- 'D:/ROMS_SIMULATIONS/peru02km/'
# nc            <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[1])
# kmdist        <- 51 # Distancia en pixeles desde la costa
# pixinterval   <- 2 # Intervalo de pixeles respecto a la costa
# lat_1         <- -6.5 # Latmxax
# lat_2         <- -13.5 # Latmin
# maxdepth      <- 50 # Profundidad maxima para posicionar los drifters
# intervaldepth <- 5 # Intervalo de profundaid para los drifters
# 
# #-------------- Do not change anything after here----------------#
# mask <- ncvar_get(nc, 'mask_rho'); mask2 <- mask     # matrix
# lon  <- ncvar_get(nc, 'lon_rho')   # matrix
# lat  <- ncvar_get(nc, 'lat_rho')   # matrix
# h    <- ncvar_get(nc, 'h')
# 
# x11()
# image.plot(lon,lat, mask, xlim = c(-88,-72))
# points(lonlat[,1], lonlat[,2], pch = 1, cex = 0.1)


# 
# dat <- read.table('E:/ICHTHYOP/peru10km/DistCoast/cfg/peru_drifters.txt')
# dat <- dat[-c(18380),]
# write.table(x = dat, 'E:/ICHTHYOP/peru10km/DistCoast/cfg/peru_drifters.txt', col.names = F, row.names = F)
# 

# ncfile = NULL
# firstdrifter = 1
# lastdrifter = 19448
# firsttime = 1
# lasttime = 31
# recruitmentzone = 1
# dirpath   <- 'E:/ICHTHYOP/peru10km/DistCoast/out/drifters/'
# new_path  <- 'E:/ICHTHYOP/peru10km/DistCoast/cfg/'
# dates           <- read.table(paste0(new_path, 'date_scrum_time_ichthyop.csv'), header = T, sep = ';')
# xy <- read.table(paste0(new_path, 'lonlatDrifters.csv'), sep = ';')


