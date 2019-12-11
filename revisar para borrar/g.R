dirpath <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/mesoXd/10/'

len_serie <- NULL
for(i in 1:12){
  Rfile <- paste0(dirpath, '/results/trajectoriesM', i,'.Rdata')
  load(file = Rfile)
  print(Rfile)
  len <- tapply(trajectories$Length, trajectories$Timer, mean)
  len_serie <- cbind(len_serie, len)
}

png(filename = paste0(dirpath, '/results/growth_curves.png'), height = 850, width = 850, res = 120)
col = 0
plot(1:31, ylim = c(0,4), type = 'n', xlab = 'Days', ylab = 'Length (mm)')
for(i in 1:12){
  col = col + 1
  lines(1:31, len_serie[,i], col = col)
}
dev.off()

# library(ncdf4)
# library(fields)
# nc <- nc_open('E:/ROMS_SILUMATIONS/10kmparent/roms_avg_Y2012M1.Jaard10kmClim.nc')
# 
# lon  <- ncvar_get(nc = nc, varid = 'lon_rho')
# lat  <- ncvar_get(nc = nc, varid = 'lat_rho')
# mask <- ncvar_get(nc = nc, varid = 'mask_rho'); mask[mask == 0] <- NA
# 
# NANO <- ncvar_get(nc = nc, varid = 'NANO')[,,64,1]
# ZOO  <- ncvar_get(nc = nc, varid = 'ZOO') [,,64,1]
# MESO <- ncvar_get(nc = nc, varid = 'MESO')[,,64,1]
# DIA  <- ncvar_get(nc = nc, varid = 'DIA')[,,64,1]
# 
# zlim <- c(0,15)
# x11();par(mfrow = c(2,2))
# image.plot(lon, lat, NANO*mask, zlim = zlim)
# image.plot(lon, lat, ZOO*mask, zlim = zlim)
# image.plot(lon, lat, MESO*mask, zlim = zlim)
# image.plot(lon, lat, DIA*mask, zlim = zlim)


# boxplot(trajectories$Length ~ trajectories$Timer)

# library(ncdf4)
# nc <- nc_open('E:/ICHTHYOP/10kmparent/Fisica-DEB/out/nano/out_ichthyop-run201911261511_s1.nc')
# 
# # talla_name: particle length
# # unit: millimeter
# talla <- ncvar_get(nc, 'length')
# 
# meso <- talla[,31]


# # talla_name: particle biological stage
# # unit: numerical code
# # EGG: 0
# # YOLK_SAC_LARVA: 1
# # FEEDING_LARVA: 2
# stage <- ncvar_get(nc, 'stage')

# # talla_name: particle energy reserve
# # unit: J
# E <- ncvar_get(nc, 'E')


length(seq(from = as.Date('2019-9-1'), to = as.Date('2019-12-22'), by = 'day'))
a <- read.table('C:/Users/jflores/Desktop/Data002.txt')
write.table(x = a, file = 'C:/Users/jflores/Desktop/Data002.csv', sep = ';', col.names = F, row.names = F)

