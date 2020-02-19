source('source/plot_traj_ggmap_depth.R')

dirpath <- 'E:/ICHTHYOP/peru02km/LatitudeBathy/out/'


load(paste0(dirpath, '/results/trajectoriesM6.Rdata'))
dat <- trajectories
dat$Depth[dat$Depth > 0] <- 0
dat <- subset(dat, dat$IfRecruited == 0)

for(i in 1:10){
  dat$Zone_name[grep(pattern = paste0('zone', i), x = dat$Zone_name)] <- paste0('zone', i)
}

levels(factor(dat$ReleaseDepth))
levels(factor(dat$ReleaseBathy))
levels(factor(dat$Zone_name))

bathy1 <- subset(dat, dat$ReleaseBathy == '0-50')
bathy2 <- subset(dat, dat$ReleaseBathy == '50-100')
bathy3 <- subset(dat, dat$ReleaseBathy == '100-200')
bathy4 <- subset(dat, dat$ReleaseBathy == '200-600')
bathy5 <- subset(dat, dat$ReleaseBathy == '600-2000')

for(i in 1:5){
  for(j in 1:4){
    PNG <- paste0(dirpath,'/results/', 'bathy', levels(factor(dat$ReleaseBathy))[i], 'latitude', levels(factor(dat$Zone_name))[j], '.png')
    toplot <- subset(dat, dat$Zone_name == levels(factor(dat$Zone_name))[j] & dat$ReleaseBathy == levels(factor(dat$ReleaseBathy))[i])
    plot_traj_ggmap_depth(df = toplot, pngfile = PNG, zlim = c(-60,0), xlim = c(-85,-75), ylim = c(-15,-5))
  }
}




par(mfrow = c(1,3))
hist(trajectories$temp)
hist(trajectories$MESO)
hist(trajectories$O2)

x11()
par(mfrow = c(1,3), mar = c(5,4,1,1))
d1 <- density(trajectories$temp) # returns the density data
d2 <- density(trajectories$MESO) # returns the density data
d3 <- density(trajectories$O2) # returns the density data

plot(d1, xlim = c(12, 26), main = 'ºC') # plots the results
plot(d2, xlim = c(0, 11), main = 'umol C L-1') # plots the results
plot(d3, xlim = c(0, 300), main = 'O2 umol L-1') # plots the results


nc <- nc_open('D:/ROMS_SILUMATIONS/10kmparent/roms_avg_Y2012M1.Jaard10kmClim.nc')
