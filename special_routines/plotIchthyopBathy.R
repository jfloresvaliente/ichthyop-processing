library(ncdf4)
library(fields)

dirpath <- 'D:/ROMS_SILUMATIONS/10kmparent/'

nc <- list.files(path = dirpath, pattern = '.nc', full.names = T)[1]
nc <- nc_open(filename = nc)

lon  <- ncvar_get(nc, 'lon_rho')
lat  <- ncvar_get(nc, 'lat_rho')
mask <- ncvar_get(nc, 'mask_rho')
h    <- ncvar_get(nc, 'h')

xy <- as.matrix(read.table(paste0(dirpath, 'interpolated/getzone_rowcol_index.txt')))

# Plot zona de retencion
for(i in 1:dim(xy)[1]){
  row_val <- xy[i,]
  mask[ row_val[1] , row_val[2] ] <- NA
}

cols <- c('black','blue')

png_name <- paste0(dirpath, 'interpolated/getzone_rowcol_index.png')
png(filename = png_name, width = 850, height = 850, res = 120)
par(lwd = 2)
image.plot(lon, lat, mask,
           xlab = '', ylab = '',
           col = cols, axes = F)
axis(side = 1, font = 2, lwd = 2)
axis(side = 2, font = 2, lwd = 2, las = 2)
mtext(side = 1, text = 'LON', font = 2, line = 3)
mtext(side = 2, text = 'LAT', font = 2, line = 3)
box()
dev.off()

# Plot zona de retencion: 3 batimetrias
h[h > 0   & h <= 100]  <- 2
h[h > 100 & h <= 500]  <- 3
h[h > 500 & h <= 3500] <- 4
h[h > 3500] <- 1

mask <- ncvar_get(nc, 'mask_rho')
for(i in 1:dim(xy)[1]){
  row_val <- xy[i,]
  mask[ row_val[1] , row_val[2] ] <- 2
}
mask[mask == 1] <- NA

cols <- c('black','red','blue','green')
png_name <- paste0(dirpath, 'interpolated/getzone_rowcol_index3bati.png')
png(filename = png_name, width = 850, height = 850, res = 120)
par(lwd = 2)
image.plot(lon, lat, mask*h,
           xlab = '', ylab = '',
           col = cols, axes = F)
axis(side = 1, font = 2, lwd = 2)
axis(side = 2, font = 2, lwd = 2, las = 2)
mtext(side = 1, text = 'LON', font = 2, line = 3)
mtext(side = 2, text = 'LAT', font = 2, line = 3)
abline(h = seq(-2,-20,-2), lty = 2, col = 'grey80', lwd = .5)
box()
dev.off()

