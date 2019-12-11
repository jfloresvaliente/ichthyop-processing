library(ncdf4)
library(fields)
library(maps)
library(mapdata)

nc <- nc_open('E:/ROMS_SIMULATIONS/peru10km/roms_avg_Y2008M1.AscatMerClim.nc')

lon <- ncvar_get(nc, 'lon_rho')
lat <- ncvar_get(nc, 'lat_rho')
mask <- ncvar_get(nc, 'mask_rho')
# mask[mask == 0] <- NA
h <- ncvar_get(nc, 'h')
# h[h > 100] <- NA
# x11()
# image.plot(lon, lat, h*mask, axes = F, ylim = c(-20,-2))
# axis(2, at = seq(-20, 15, 2), labels = seq(-20, 15, 2))
# abline(h = seq(-20, 15, 2))
# grid()

nlevels <- c(0,100,500,3000)
zlim <- c(0,5000)
lev <- seq(from = 0, to = zlim[2], length.out = nlevels)
filled.contour(
  # x = seq(from = -96, to = -70, by = 1/6),
  #              y = seq(from = -22, to = -5,  by = 1/6),
               z = h,
               drawlabels = TRUE,
               # plot.axes = {axis(1, 1:12)
               #   axis(2, seq(latilim[1],latilim[2], by = 2))},
               zlim = zlim, col = tim.colors(length(lev)-1), levels = lev, xlab = 'LON', ylab = 'LAT')

x11()
contour(x = lon[,1], y = lat[1,], z = h, levels = c(0,100,500,3000))
map('worldHires', add = T, fill = T, col = 'grey')
