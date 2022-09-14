#=============================================================================#
# Name   : plotIchthyopZones_trajs
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Plot latitudinal zones for Ichthyop simulations
# URL    : 
#=============================================================================#
library(ncdf4)
library(fields)
library(maps)
library(mapdata)

dirpath <- 'D:/ROMS_SILUMATIONS/10kmparent/'
nc_file <- list.files(path = dirpath, pattern = '.nc', full.names = T)[1]
nc      <- nc_open(nc_file)
lon     <- ncvar_get(nc, 'lon_rho')
lat     <- ncvar_get(nc, 'lat_rho')
h       <- ncvar_get(nc, 'h')
nc_close(nc)

latis <- c(-20,-2) # Latitudes de menor a mayor
longi <- c(-82.5, -70) # Longitudes de menor a mayor
bathy <- 2000 # Máxima batimería

dir_ich <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052/case1/results/'
#=================================#
# Get bathimetry between 0-2000 m
h2 <- h
h2[h2 > 0 & h2 <= bathy] <- NA
h2[!is.na(h2)] <- 0
h2[is.na(h2)]  <- 1
h2[h2 == 0] <- NA

# Get longitudinal zone [-82.5 & -70 Wº]
lon2 <- lon
lon2[lon2 >= longi[1] & lon2 <= longi[2]] <- NA
lon2[!is.na(lon2)] <- 0
lon2[is.na(lon2)]  <- 1
lon2[lon2 == 0] <- NA

# Get latitudinal zone [-20 & -2 Sº]
lat2 <- lat
lat2[lat2 > latis[1] & lat2 <= latis[2]] <- NA
lat2[!is.na(lat2)] <- 0
lat2[is.na(lat2)]  <- 1
lat2[lat2 == 0] <- NA

# Sum of previous zones
m <- lon2 + lat2 + h2; rm(lon2, lat2, h2)
m[!is.na(m)] <- 1
m[is.na(m)]  <- 0

m2 <- m
new_mask <- m

# Get zones by 2º
lat_in <- seq(latis[2], latis[1]+2, -2)
lat_on <- lat_in - 2

for(i in seq_along(lat_in)){
  lat2 <- lat
  lat2[lat2 > lat_on[i] & lat2 <= lat_in[i]] <- NA
  lat2[!is.na(lat2)] <- 0
  lat2[is.na(lat2)]  <- i
  lat2 <- lat2 * m2
  
  new_mask <- new_mask + lat2
}

new_mask[new_mask == 0] <- NA
new_mask <- new_mask - 1

# Puntos extremos del dominio ROMS 2 km
x <- c(-87.12209, -80.69843, -73.90596, -80.45255)
y <- c(-7.570634, -4.086924, -15.852085, -19.427472)

x_name <- c(-80.87991, -81.25923, -80.42472, -79.43849, -78.22467, -77.23843, -75.87288, -72.45900, -70.71412)
y_name <- c(-3.096295, -5.235192, -7.179643, -9.075484, -11.068547, -12.867165, -15.054674, -17.242182, -18.603298)

colores <- tim.colors(n = length(lat_in)+2, alpha = 1)[c(2:(length(lat_in)+1))]

#-------- Ichthyop trajs --------#
load(file = paste0(dir_ich, 'trajectoriesM1.Rdata'))
dat <- trajectories
rec_ins <- sample(x = subset(dat, dat$IfRecruited == 1 & dat$Timer == 91)$Drifter, size = 10)
rec_ins <- subset(dat, dat$Drifter %in% rec_ins)
rec_out <- sample(x = subset(dat, dat$IfRecruited == 0 & dat$Timer == 91)$Drifter, size = 10)
rec_out <- subset(dat, dat$Drifter %in% rec_out)
mat_rec <- rbind(rec_ins, rec_out)

png(filename = 'C:/Users/jflores/Desktop/ich_zones_map3.png', width = 850, height = 850, res = 120)
par(mar = c(4.5,4.5,.5,.5))
image.plot(lon, lat, new_mask, xlab = '', ylab = '', xlim = c(-90,-70), ylim = c(-20,0),
           axes = F, col = colores,
           legend.width = 0.00000001, axis.args = list(cex.axis = 0.00000001, font.axis = 2))
# polygon(x = x, y = y, lty = 2, border = 'grey25', angle = 45, lwd = 2)
map('worldHires', add=T, fill=T, col='grey')
contour(lon[,1], lat[1,], h, levels = c(100, 500,2000), add = T, lty = 2, labcex = .65)
axis(side = 1, font = 2, lwd = 2, cex.axis = 1.5)
axis(side = 2, font = 2, lwd = 2, cex.axis = 1.5, las = 2)
box(lwd = 2)
mtext(side = 1, font = 2, line = 2.8, cex = 1.5, text = 'Longitude')
mtext(side = 2, font = 2, line = 2.8, cex = 1.5, text = 'Latitude')
text(x = x_name, y = y_name, labels = 1:9, font = 2, cex = 1.1, col = 'black')

drifters <- levels(factor(mat_rec$Drifter))
for(i in 1:length(drifters)){
  sub_points <- subset(dat, dat$Drifter == drifters[i])
  lines(sub_points$Lon, sub_points$Lat)
}
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#