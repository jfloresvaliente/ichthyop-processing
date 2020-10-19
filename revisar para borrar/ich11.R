dirpath <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/MESO60dias/results/'

for(i in 1:12){
  load(paste0(dirpath, '/trajectoriesM', i,'.Rdata'))
  
  dat <- trajectories; rm(trajectories)
  # dat <- subset(dat, dat$Drifter %in% c(5001:7500))

  last <- subset(dat, dat$Timer == 721 & dat$IfRecruited == 1)
  last <- last$Drifter
  last <- sample(x = last, size = 50)

  dat <- subset(dat, dat$Drifter %in% last)
  dat$Drifter <- rep(1:50, each = 721)
  print(range(dat$temp))
  dat <- dat[,-c(7:10)]

  RData <- paste0(dirpath, 'dat', i, '.csv')
  print(paste0('saving .....', RData))
  write.table(x = dat, file = RData, row.names = F, col.names = names(dat), sep = ';')
  
  # save(dat, file = RData)
}

# library(biogeo)
# dd<-c(23,45,34)
# mm<-c(45,34,22)
# ss<-c(2,56,10)
# ns<-c("E","W","N")
# a <- dms2dd(dd,mm,ss,ns)



# library(ncdf4)
# nc <- nc_open('D:/ROMS_SIMULATIONS/peru10km/roms_avg_Y2011M12.AscatMerClim.nc')
# 
# ncvar_get(nc, 'scrum_time')
# ncvar_get(nc, 'lon_rho')
# 
# 
# 
# dat <- read.table(file = 'C:/Users/jflores/Desktop/lat_lon_nutrias.csv', header = T, sep = ',')
