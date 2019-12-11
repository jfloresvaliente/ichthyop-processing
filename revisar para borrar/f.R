library(ncdf4)
library(fields)

nc <- nc_open('D:/ROMS_SIMULATIONS/peru02km/roms_avg_Y2008M1.newperushtopoP.nc')

lon <- ncvar_get(nc, 'lon_rho')
lat <- ncvar_get(nc, 'lat_rho')
mask<- ncvar_get(nc, 'mask_rho')
angle <- ncvar_get(nc, 'angle')

# x11();
image.plot(lon, lat, mask)
# segments(x0 = -80.75, y0 = -4.25, x1 = -74.3, y1 = -16)
abline(h = c(-6,-15.5))
# locator(n = 2, type = 'p')

segments(x0 = -84.0, y0 = -6, x1 = -79.0, y1 = -15.5)
segments(x0 = -83.5, y0 = -6, x1 = -78.5, y1 = -15.5)
segments(x0 = -83.0, y0 = -6, x1 = -78.0, y1 = -15.5)
segments(x0 = -82.5, y0 = -6, x1 = -77.5, y1 = -15.5)
segments(x0 = -82.0, y0 = -6, x1 = -77.0, y1 = -15.5)
segments(x0 = -81.5, y0 = -6, x1 = -76.5, y1 = -15.5)
segments(x0 = -81.0, y0 = -6, x1 = -76.0, y1 = -15.5)
segments(x0 = -80.5, y0 = -6, x1 = -75.5, y1 = -15.5)
segments(x0 = -80.0, y0 = -6, x1 = -75.0, y1 = -15.5)








