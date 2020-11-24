#=============================================================================#
# Name   : plot_hovmuller_recruited_byLat
# Author : 
# Date   : 
# Version:
# Aim    : Calculates recruitment at higher spatial (spawning latitude) and temporal (spawning frequency) resolution.
# URL    : 
#=============================================================================#
library(fields)

dirpath       <- 'C:/Users/jflores/Desktop/ICHTHYOP/peru10km/Brochier2008/LatitudeDepthBathy/out/results/'
latilim       <- c(-20, -2)   # Latitude extension of the spawning zone
lat_div       <- 2          # Latitudinal resolution
computeattime <- 31           # Step time to calculate larval retention
lastdrifter   <- 5000         # Number of particles released in each simulation
year_rep      <- 3            # Number of years (in case of climatological approach)

# Plot Hovmuller
zlim     <- 70            # Retention rate interval to be plotted
nlevels  <- 25            # Number of levels in the color palette
isolines <- seq(0,zlim,5) # Isolines to be plotted

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

lat_ini <- seq(latilim[1], latilim[2], lat_div)
lat_out <- lat_ini + lat_div

m <- NULL
for(j in 1:12){
  Rdata <- paste0(dirpath,'trajectoriesM',j,'.Rdata')
  load(Rdata)
  print(Rdata)
  dat <- trajectories; rm(trajectories)
  
  dat_ini <- subset(dat, dat$Timer == 1)
  
  drif_end <- seq(from = 0, to = dim(dat_ini)[1], by = lastdrifter)
  drif_ini <- drif_end + 1
  
  k_releaed <- NULL
  k_recruit <- NULL
  for(k in 1:(length(drif_ini)-1)){
    drif_sub <- dat_ini[c(drif_ini[k]:drif_end[k+1]) , ]
    
    a <- NULL
    b <- NULL 
    for(i in 1:(length(lat_ini)-1)){
      lat_sub <- subset(drif_sub, drif_sub$Lat >= lat_ini[i] & drif_sub$Lat < lat_out[i])
      
      released  <- dim(lat_sub)[1]
      recruited <- dim(subset(dat, dat$Drifter %in% lat_sub$Drifter & dat$Timer == computeattime & dat$IfRecruited ==1))[1]
      
      a <- c(a, released)
      b <- c(b, recruited)
    }
    k_releaed <- cbind(k_releaed, a)
    k_recruit <- cbind(k_recruit, b)
  }

  k_releaed1 <- k_releaed[,c(1:3)]
  k_releaed2 <- k_releaed[,c(4:6)]
  k_releaed3 <- k_releaed[,c(7:9)]
  k_releaed  <- k_releaed1 + k_releaed2 + k_releaed3
  
  k_recruit1 <- k_recruit[,c(1:3)]
  k_recruit2 <- k_recruit[,c(4:6)]
  k_recruit3 <- k_recruit[,c(7:9)]
  k_recruit  <- k_recruit1 + k_recruit2 + k_recruit3
  
  percent <- (k_recruit/k_releaed)*100
  m <- cbind(m, percent)
}

z <- t(m)
x <- seq(from = 1, to = 12, length.out = 12*year_rep)
y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])
lev <- seq(from = 0, to = zlim, length.out = nlevels)

png(filename = paste0(dirpath, 'hovmullerRecruited_revisado.png'), width = 850, height = 850, res = 120)
filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1), levels = lev,
               xlab = 'Months', ylab = 'Latitude',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
                 axis(1, 1:12)
                 axis(2, seq(latilim[1],latilim[2], by = 2))
               })
dev.off()

png(filename = paste0(dirpath, 'hovmullerRecruitedImagePlot_revisado.png'), width = 850, height = 850, res = 120)
image.plot(x,y,z, axes = F, xlab = 'Months', ylab = 'Latitude', zlim = c(0, zlim))
axis(1)
axis(2)
box()
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#