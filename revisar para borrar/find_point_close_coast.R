library(ncdf4)
library(fields)

nc_file <- 'C:/Users/jflores/Desktop/roms6b_avg.Y1978.M3.rsodi1.nc'
lon <- -77.6363
lat <- -10.42947

# No cambiar desde aqui
nc       <- nc_open(nc_file)
lon_rho  <- ncvar_get(nc, 'lon_rho')
lat_rho  <- ncvar_get(nc, 'lat_rho')
mask_rho <- ncvar_get(nc, 'mask_rho')

xlim <- c(lon-1.5 , lon+1.5)
ylim <- c(lat-1.5 , lat+1.5)

X11()
image.plot(lon_rho,lat_rho,mask_rho, xlim = xlim, ylim = ylim, xlab = 'LON', ylab = 'LAT')
points(lon,lat)
locator(n = 1, type = 'p')
