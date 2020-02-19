dirpath   <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/lati220/results_fisica/'
dat <- read.csv(paste0(dirpath, 'ichthyop_output.csv'), sep = ';')

# seq(-20,-2,6)


lat1 <- -20
lat2 <- -2

# dat <- subset(dat, dat$Lat >= lat1 & dat$Lat < lat2)

lati1 <- seq(lat1, lat2-1, 1)
lati2 <- lati1 + 1

month_per <- NULL
for(j in 1:12){
  month <- subset(dat, dat$Day == j)
  
  lat <- NULL
  for(i in 1:length(lati1)){
    reg <- subset(month, month$Lat >= lati1[i] & month$Lat < lati2[i])
    per <- (sum(reg$IfRecruited)*100)/dim(reg)[1]
    lat <- c(lat, per)
  }
  month_per <- rbind(month_per, lat)
}

# dirpath <- 'E:/ICHTHYOP/10kmparent/Fisica/out/results/'
# depthlim <- c(-30, -1)   # Latitude extension of the area 
zlim     <- c(25)        # Retention rate interval to be plotted
nlevels  <- 25           # Number of levels in the color palette
isolines <- seq(0,zlim,5) # Isolines to be plotted

#------------- Do not change anything after here -------------#
latis <- (lati1+lati2)/2
lev <- seq(from = 0, to = zlim, length.out = nlevels)

x <- 1:12
y <- seq(from = lat1, to = lat2, length.out = dim(month_per)[2])
z <- month_per

png(filename = paste0(dirpath, 'hovmuller.png'), width = 850, height = 850, res = 120)
filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1),levels = lev,
               xlab = 'Months', ylab = 'Latitude',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
                 axis(1, 1:12)
                 axis(2, latis)
               })
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#