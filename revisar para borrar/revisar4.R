load('D:/ICHTHYOP/10kmparent/Fisica-DEB/out/MESO/X_d0.1/length20mm/results/trajectoriesM1.Rdata')
dat <- trajectories; rm(trajectories)

particle <- subset(dat, dat$Drifter == dat[which.max(dat$length),1])
particle <- subset(dat, dat$Drifter == 200)

plot(particle$length, type = 'l', axes = T)
# axis(2)
# axis(1, at = seq(from = 1, length.out = 30, by = 12), labels = seq(from = 0, length.out = 30, by = 1))

plot(particle$E, type = 'l')
plot(particle$temp, type = 'l')
plot(particle$MESO, type = 'l')

dens <- density(dat$MESO)
plot(dens)


prey <- particle$MESO
x_d <- mean(prey)
f_serie <- NULL
for(i in 1:length(prey)){
  x <- prey[i]
  f <- x/(x+x_d)
  f_serie <- c(f_serie, f)
}
plot(f_serie, type = 'l')

########################--
last <- subset(dat, dat$Timer == 31)
hist(last$length)
