dirpath   <- 'E:/ICHTHYOP/peru10km/Brochier2008/LatitudeDepthBathy/out/results/'
load(paste0(dirpath, 'trajectoriesM', 1, '.RData'))

dat <- trajectories
dat$Depth <- abs(dat$Depth) * -1

bathylev <- levels(factor(dat$ReleaseBathy))

recluta1 <- NULL
recluta0 <- NULL
for(i in 1:length(bathylev)){
  bathy <- subset(dat, dat$ReleaseBathy == bathylev[i])
  si <- subset(bathy, bathy$IfRecruited == 1)
  no <- subset(bathy, bathy$IfRecruited == 0)
  
  si <- tapply(si$Depth, si$Timer, mean)
  no <- tapply(no$Depth, no$Timer, mean)
  
  recluta1 <- cbind(recluta1, si)
  recluta0 <- cbind(recluta0, no)
}

x11()
pch <- 15:17
plot(1:31, type = 'n', ylim = c(-30, -10), xlab = 'Days', ylab = 'Depth (m)')
for(i in 1:length(bathylev)){
  lines(recluta1[,i], type = 'o', pch = pch[i], col = 'red')
  lines(recluta0[,i], type = 'o', pch = pch[i], col = 'blue')
}
legend('topleft' , legend = bathylev, bty = 'n', pch = pch, lty = 1, col = 'red' , title = 'Retained')
legend('topright', legend = bathylev, bty = 'n', pch = pch, lty = 1, col = 'blue', title = 'No-Retained')

