load('C:/Users/jflores/Desktop/ichthyop-test/out/age_criterion_DEB/results/trajectoriesM1.Rdata')
DEB <- trajectories; rm(trajectories)

load('C:/Users/jflores/Desktop/ichthyop-test/out/length_criterion_DEB_growth/results/trajectoriesM1.Rdata')
DEB_growth <- trajectories; rm(trajectories)

DEB <- subset(DEB, DEB$ReleaseDepth == '15-30' & DEB$ReleaseBathy == '0-100' & DEB$ReleaseArea == 4)
DEB_growth <- subset(DEB_growth, DEB_growth$ReleaseDepth == '15-30' & DEB_growth$ReleaseBathy == '0-100' & DEB_growth$ReleaseArea == 4)

D1 <- subset(DEB, DEB$Drifter == 24)
D2 <- subset(DEB_growth, DEB_growth$Drifter == 21)

# plot(D1$E, type = 'l')
# plot(D1$length, type = 'l')
# plot(D1$MESO, type = 'l')
# plot(D1$temp, type = 'l')

x11()
plot(1:31, ylim = c(0,40), type = 'n', xlab = 'Days', ylab = 'Length')
lines(D1$length, type = 'l')
lines(D2$length, type = 'l', col = 'red')

legend('topleft', legend = c('DEB', 'DEB + linear growth'), bty = 'n', col = c('black','red'), lty = 1, text.col = c('black','red'))
