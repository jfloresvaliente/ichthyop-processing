#=============================================================================#
# Name   : get_recruitment_zone_mask
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Get the mask for recruitment zone from ROMS file
# URL    : 
#=============================================================================#

# Due to the bug present in Ichthyop-DEB, it is not possible to apply
# a size-based recruitment test while the model is running.
# To solve this, it is necessary to do this test as a post processing analysis.

source('source/libraries.R')

roms_file  <- 'E:/ROMS_SILUMATIONS/10kmparent/roms_avg_Y2012M1.Jaard10kmClim.nc'
xml_file   <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/cfg/retention.xml'
resolution <- 1/7
outpath    <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/cfg/'

# Do not change anything after here
nc       <- nc_open(filename = roms_file)
filezone <- xmlTreeParse(xml_file, useInternalNode = TRUE)

inshore  <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/bathy_mask/line_inshore'))
inshore  <- as.numeric(as.character(inshore[,1]))
offshore <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/bathy_mask/line_offshore'))
offshore <- as.numeric(as.character(offshore[,1]))

point_lon <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/polygon/point/lon'))
point_lon <- as.numeric(as.character(point_lon[,1]))
point_lat <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/polygon/point/lat'))
point_lat <- as.numeric(as.character(point_lat[,1]))

x  <- ncvar_get(nc, 'lon_rho')
y  <- ncvar_get(nc, 'lat_rho')
z  <- ncvar_get(nc, 'mask_rho')
h  <- ncvar_get(nc, 'h') * z
xy <- cbind(as.vector(x), as.vector(y))

inout <- in.out(bnd = cbind(point_lon, point_lat), x = xy)
inout <- matrix(data = inout, nrow = dim(z)[1], ncol = dim(z)[2])
inout <- inout*1

h[h > offshore] <- NA
# h[h < inshore] <- NA
h[!is.na(h)] <- 1
h[is.na(h)] <- 0

z <- inout + h
z[z != 2] <- 0
z[z == 2] <- 1

# Define new lon-lat values for new grid (by = indicates spatial resolution in degrees)
xlim <- round(range(x))
ylim <- round(range(y))

x0 <- seq(from = xlim[1], to = xlim[2], by = resolution)
y0 <- seq(from = ylim[1], to = ylim[2], by = resolution)

new_dat  <- interp(x, y, z   , xo = x0, yo = y0)

z <- new_dat[[3]]; z[z > 0] <- 1
x <- matrix(data = new_dat$x, byrow = F, nrow = dim(new_dat$z)[1], ncol = dim(new_dat$z)[2])
y <- matrix(data = new_dat$y, byrow = T, nrow = dim(new_dat$z)[1], ncol = dim(new_dat$z)[2])

xyz <- cbind(as.numeric(x), as.numeric(y), as.numeric(z))
r   <- rasterFromXYZ(xyz = xyz)
SP  <- rasterToPolygons(clump(r==1), dissolve=TRUE)
k   <- SP@polygons[[1]]@Polygons[[1]]@coords

# image.plot(x,y,z)
# polygon(x = polyg[,1], y = polyg[,2])
# abline(h = c(-2,-20))

write.table(x = k, file = paste0(outpath, 'ichthyop_recruitment_polygon.txt'), col.names = F, row.names = F)
# in.out(bnd = k, x = xy)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#