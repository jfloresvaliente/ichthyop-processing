#=============================================================================#
# Name   : plot_hovmuller_recruited_age
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Hovmuller diagram of the retention rates
# URL    : 
#=============================================================================#
library(fields)
library(ggplot2)
library(stringr)
library(reshape2)
library(hexbin)

dirpath  <- 'E:/ICHTHYOP/10kmparent/FISICA/out/results/'
days     <- 60
latilim  <- c(-20, -2)    # Latitude extension of the area 
zlim     <- c(30,40)            # Retention rate interval to be plotted
nlevels  <- 25            # Number of levels in the color palette
isolines <- seq(zlim[1],zlim[2],2) # Isolines to be plotted
xlim     <- c(-85, -70) # Londitude
ylim     <- c(-20, 0)   # Latitude
#========== Do not change anything after here ==========#
df_end <- NULL # Data frame for final positions of recruited particles

for(j in 1:12){
  Rdata <- paste0(dirpath,'trajectoriesM',j,'.Rdata')
  load(Rdata)
  print(Rdata)
  dat <- trajectories; rm(trajectories)
  
  index <- subset(dat, dat$Timer == (days+1) & dat$IfRecruited == 1)$Drifter
  
  recruited <- subset(dat, dat$Drifter %in% index)
  recruited <- matrix(data = recruited$IfRecruited, ncol = (days+1), nrow = dim(recruited)[1]/(days+1), byrow = T)
  recruited_day    <- apply(recruited, 1, which.max)
  
  for(u in seq_along(recruited_day)){
    inicol <- recruited_day[u]
    
    if(inicol == days+1) next()
    recruited[u, (inicol+1) : dim(recruited)[2]] <- NA
  }
  
  recruited <- as.vector(t(recruited))
  
  dat <- subset(dat, dat$Drifter %in% index)
  dat <- cbind(dat, recruited)
  
  dat <- subset(dat, dat$recruited == 1)
  dat$AgeRecruited <- recruited_day-1
  dat$Month <- j
  
  for(i in 1:9){
    zone_name <- paste0('zone', i)
    a <- grep(pattern = zone_name, x = dat$Zone_name)
    dat$Zone_name[a] <- zone_name
  }
  
  # Get an aleatory sample of 1000 particles
  # set.seed(10^2)
  # dat <- dat[sample(nrow(dat), 1000), ]
  
  names_col <- c('Drifter', 'Lon', 'Lat', 'Depth', 'Zone_name', 'ReleaseDepth', 'ReleaseBathy', 'AgeRecruited', 'Month')
  names_ind <- NULL
  for(n in seq_along(names_col)){
    names_ind <- c(names_ind, which(names(dat) == names_col[n]))
  }
  dat <- dat[, names_ind]
  
  df_end <- rbind(df_end, dat)
  

  rm(dat)
}
df_end <- df_end[,-c(1)]; df_end$Month <- as.factor(df_end$Month)
dat <- df_end; rm(df_end)

# Plotear el promedio general
z <- tapply(dat$AgeRecruited, list(dat$Month, dat$Zone_name), FUN = mean, na.rm = T)
z <- z[, c(dim(z)[2]:1)]
x <- 1:12
y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])

lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels)

png(filename = paste0(dirpath, 'hovmullerRecruited_Age.png'), width = 850, height = 850, res = 120)
filled.contour(x = x, y = y, z = z, zlim = zlim, col = tim.colors(length(lev)-1), levels = lev,
               xlab = 'Months', ylab = 'Latitude',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
                 axis(1, 1:12)
                 axis(2, seq(latilim[1],latilim[2], by = 2))
               })
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#

# colnames(z) <- 9:1
# rownames(z) <- 1:12
# 
# z <- melt(z)
# 
# p <- ggplot(z, aes(x = Var1, y = Var2, z = value, colour = stat(level))) +
#   geom_contour_filled()
# p
# 
# dat$mes <- as.numeric(as.character(dat$Month))
# dat$zona <- as.numeric(str_remove(string = dat$Zone_name, pattern = 'zone'))
# 
