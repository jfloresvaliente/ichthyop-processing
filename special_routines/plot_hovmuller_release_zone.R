source('ichthyop_libraries.R')

dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/interpolatedYearMonth/'
sufijo  <- 'ReleaseZone'
nlevels <- 64 # Number of levels in the color palette
by_lat  <- 2

#===== Config for temp var =====#
namevar  <- 'TEMP'
zlim     <- c(14, 22)
isolines <- seq(zlim[1], zlim[2], 1) # Isolines to be plotted
caption  <- 'Temperature [ºC]'

# #===== Config for MESO var =====#
# namevar  <- 'MESO'
# zlim     <- c(0, 4.5)
# isolines <- seq(zlim[1], zlim[2], .5) # Isolines to be plotted
# caption  <- 'Mesozooplankton [umol C L-1]'

# #===== Config for functional response (f) var =====#
# namevar  <- 'MESOf'
# zlim     <- c(0.25, 0.7)
# isolines <- seq(zlim[1], zlim[2], .05) # Isolines to be plotted
# caption  <- 'Functional response [#]'

# #===== Config for salt var =====#
# namevar  <- 'SALT'
# zlim     <- c(34.3, 35.17)
# isolines <- seq(zlim[1], zlim[2], 0.1) # Isolines to be plotted
# caption  <- 'Salinity [PSU]'

# #===== Config for O2 var =====#
# namevar  <- 'O2'
# zlim     <- c(50, 225)
# isolines <- seq(zlim[1], zlim[2], 20) # Isolines to be plotted
# caption  <- 'Oxygen [umol L-1]'

# #===== Config for V var =====#
# namevar  <- 'V'
# zlim     <- c(-0.025, 0.050)
# isolines <- round(seq(zlim[1], zlim[2], 0.01), 3) # Isolines to be plotted
# caption  <- 'Velocity V [m/s]'

# #===== Config for U var =====#
# namevar  <- 'U'
# zlim     <- c(-0.12, 0.12)
# isolines <- round(seq(zlim[1], zlim[2], 0.02), 2) # Isolines to be plotted
# caption  <- 'Velocity U [m/s]'

#======= Do not change anything from here=======#
csv_data <- paste0(dirpath, sufijo, '/',namevar,'_hovmullerReleaseZone.csv')
png_name <- paste0(dirpath, sufijo, '/',namevar,'_',by_lat,'_hovmullerReleaseZone.png')
lat      <- as.matrix(read.table(paste0(dirpath, 'lat.txt')))
hovmuller <- read.table(csv_data, header = F, sep = ';')
xy  <- read.table(paste0(dirpath, sufijo,'/','release_zone_rowcol_index.txt'))

xy$lat <- NA
for(i in 1:dim(xy)[1]){
  # print(i)
  xy$lat[i] <- lat[xy[i,1], xy[i,2]]
}

lat_in <- seq(-20, -2, by = by_lat)
lat_on <- lat_in + by_lat

lat_hov <- NULL
for(i in 1:(length(lat_in)-1)){
  # print(lat_in[i])
  row_index <- which(xy$lat >= lat_in[i] & xy$lat < lat_on[i])
  sub_hov <- apply(X = hovmuller[row_index,], MARGIN = 2, FUN = mean, na.rm = T)
  lat_hov <- rbind(lat_hov, sub_hov)
}

# Plot 1 YEAR, each 3 days
hovmuller <- t(lat_hov)
hov <- array(data = NA, dim = c(120,dim(hovmuller)[2],3))
hov[,,1] <- hovmuller[1:120,]
hov[,,2] <- hovmuller[121:240,]
hov[,,3] <- hovmuller[241:360,]
hovmuller <- apply(X = hov, MARGIN = c(1,2), FUN = mean, na.rm = T)

x <- round(seq(from = 1, to = 12, length.out = dim(hovmuller)[1]), 3)
y <- seq(from = range(lat_in)[1], to = range(lat_in)[2], length.out = dim(hovmuller)[2])

# image.plot(x,y,hovmuller)

lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels)

png(filename = png_name, width = 850, height = 850, res = 120)
par(mar = c(4.5, 4.5, 3.5, 3.5))
filled.contour(x = x, y = y, z = hovmuller, zlim = zlim,
               # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = '', ylab = '',
               plot.axes = {
                 contour(x = x, y = y, z = hovmuller, levels = isolines, labels = isolines, add = T, lwd = 2, labcex = .75)
                 axis(side = 1, font = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, at = (1:range(x)[2]))
                 axis(side = 2, font = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, at = seq(from = range(y)[1], to = range(y)[2], by = 2))
                 box(lwd = 2)
               },
               key.axes = axis(4, isolines, font = 2, lwd.ticks = 2)
)
mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.35, text = 'Month')
mtext(side = 2, line = 3.0, font = 2, cex = 1.5, adj = 0.50, text = 'Latitude')
mtext(side = 3, line = 0.2, font = 2, cex = 1.5, adj = 0.00, text = caption)

dev.off()
print(range(hovmuller))
