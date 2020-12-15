#=============================================================================#
# Name   : barplot_ichthyop_mortality
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Plot 3 filas de paneles con ichthyop output y mortalidades
# URL    : 
#=============================================================================#
source('source/ichthyop_functions.R')
dirpath <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/DEB/out/results/'
lats    <- seq(from = 2, to = 20, by = 2)
computeattime <- 61
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
df <- NULL
for(j in 1:12){
  Rdata <- paste0(dirpath,'trajectoriesM',j,'.Rdata')
  load(Rdata)
  print(Rdata)
  dat <- trajectories; rm(trajectories)
  dat <- subset(dat, dat$Timer == computeattime)
  # dat$Month <- j
  df <- rbind(df, dat)
}
df$count <- 1

for(i in 1:9){
  zone_name <- paste0('zone', i)
  a <- grep(pattern = zone_name, x = df$Zone_name)
  df$Zone_name[a] <- zone_name
}

# Calcular el numero de particulas liberadas
Depth_all <- tapply(df$count, list(df$ReleaseDepth), sum)
Bathy_all <- tapply(df$count, list(df$ReleaseBathy), sum)
Zone_all  <- tapply(df$count, list(df$Zone_name), sum)
Month_all <- tapply(df$count, list(df$Month), sum)

df2 <- df

# Calcular el N_constant constante de las particulas liberadas
Rdata <- paste0(dirpath,'data_atRecruitmentAge.Rdata')
load(Rdata)

Depth_const <- tapply(df$N_constant, list(df$ReleaseDepth), sum)
Bathy_const <- tapply(df$N_constant, list(df$ReleaseBathy), sum)
Zone_const  <- tapply(df$N_constant, list(df$Zone_name), sum)
Month_const <- tapply(df$N_constant, list(df$Month), sum)

Depth_const <- (Depth_const/Depth_all)*100
Bathy_const <- (Bathy_const/Bathy_all)*100
Zone_const  <- (Zone_const/Zone_all)*100
Month_const <- (Month_const/Month_all)*100

# Calcular el N_length basado en talla de las particulas liberadas
Depth_lengt <- tapply(df$N_length, list(df$ReleaseDepth), sum)
Bathy_lengt <- tapply(df$N_length, list(df$ReleaseBathy), sum)
Zone_lengt  <- tapply(df$N_length, list(df$Zone_name), sum)
Month_lengt <- tapply(df$N_length, list(df$Month), sum)

Depth_lengt <- (Depth_lengt/Depth_all)*100
Bathy_lengt <- (Bathy_lengt/Bathy_all)*100
Zone_lengt  <- (Zone_lengt/Zone_all)*100
Month_lengt <- (Month_lengt/Month_all)*100

# Calcular el reclutamiento de ichthyop output
dat <- read.csv(file = paste0(dirpath, 'ichthyop_output.csv'), header = T, sep = ';')
day   <- recruitment_month(dat)
depth <- recruitment_depth(dat)
bathy <- recruitment_bathy(dat)
zone  <- recruitment_zone(dat)

latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º-', lats[i] + 2, 'º'))

png(filename = paste0(dirpath, '3_4plots.png'), width = 1250, height = 850, res = 120)
par(mfrow = c(3,4), mar = c(4,3,1,.1))

ylim <- c(0,60)
barplot(day[,1], ylim = ylim); mtext(text = 'Spawning Month', side = 1, line = 2, cex = 0.75); mtext(side = 3, adj = .1, line = -1, text = 'No mortality', cex = .8)
barplot(depth[,1], ylim = ylim); mtext(text = 'Spawning Depth', side = 1, line = 2, cex = 0.75)
barplot(bathy[,1], ylim = ylim); mtext(text = 'Spawning Bathymetry', side = 1, line = 2, cex = 0.75)
p1 <- barplot(zone[,1], ylim = ylim, names.arg = F); mtext(text = 'Spawning Latitude', side = 1, line = 2, cex = 0.75)
text(p1, par('usr')[3], labels = latlab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = .85)

ylim <- c(0,4)
barplot(Month_const, ylim = ylim); mtext(text = 'Spawning Month', side = 1, line = 2, cex = 0.75); mtext(side = 3, adj = .1, line = -1, text = 'Constant mortality', cex = .8)
barplot(Depth_const, ylim = ylim); mtext(text = 'Spawning Depth', side = 1, line = 2, cex = 0.75)
barplot(Bathy_const, ylim = ylim); mtext(text = 'Spawning Bathymetry', side = 1, line = 2, cex = 0.75)
barplot(Zone_const, ylim = ylim, names.arg = F); mtext(text = 'Spawning Latitude', side = 1, line = 2, cex = 0.75)
text(p1, par('usr')[3], labels = latlab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = .85)

ylim = c(0,2)
barplot(Month_lengt, ylim = ylim); mtext(text = 'Spawning Month', side = 1, line = 2, cex = 0.75); mtext(side = 3, adj = .1, line = -1, text = 'Length-dependent mortality', cex = .8)
barplot(Depth_lengt, ylim = ylim); mtext(text = 'Spawning Depth', side = 1, line = 2, cex = 0.75)
barplot(Bathy_lengt, ylim = ylim); mtext(text = 'Spawning Bathymetry', side = 1, line = 2, cex = 0.75)
barplot(Zone_lengt, ylim = ylim, names.arg = F); mtext(text = 'Spawning Latitude', side = 1, line = 2, cex = 0.75)
text(p1, par('usr')[3], labels = latlab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = .85)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#