#=============================================================================#
# Name   : barplot_ichthyop_mortality2x4_errorbar
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Plot 3 filas de paneles con ichthyop output y mortalidades
# URL    : 
#=============================================================================#
# source('ichthyop_functions.R')
dirpath <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/10kmparent/DEBf1/k_x0_90days/out/results/'
lats    <- seq(from = 2, to = 20, by = 2)
computeattime <- 91
ylab <- 'Pre-recruitment (%)'
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
Depth_all <- tapply(df$count, list(df$ReleaseDepth, df$Year), sum)
Bathy_all <- tapply(df$count, list(df$ReleaseBathy, df$Year), sum)
Zone_all  <- tapply(df$count, list(df$Zone_name, df$Year), sum)
Month_all <- tapply(df$count, list(df$Month, df$Year), sum)

df2 <- df

# Calcular el N_constant constante de las particulas liberadas
Rdata <- paste0(dirpath,'data_atRecruitmentAge.Rdata')
load(Rdata)

Depth_const <- tapply(df$N_constant, list(df$ReleaseDepth, df$Year), sum)
Bathy_const <- tapply(df$N_constant, list(df$ReleaseBathy, df$Year), sum)
Zone_const  <- tapply(df$N_constant, list(df$Zone_name, df$Year), sum)
Month_const <- tapply(df$N_constant, list(df$Month, df$Year), sum)

Depth_const <- (Depth_const/Depth_all)*100
Depth_const_mean <- apply(Depth_const, 1, mean)
Depth_const_sd <- apply(Depth_const, 1, sd)

Bathy_const <- (Bathy_const/Bathy_all)*100
Bathy_const_mean <- apply(Bathy_const, 1, mean)
Bathy_const_sd <- apply(Bathy_const, 1, sd)

Zone_const  <- (Zone_const/Zone_all)*100
Zone_const_mean <- apply(Zone_const, 1, mean)
Zone_const_sd <- apply(Zone_const, 1, sd)

Month_const <- (Month_const/Month_all)*100
Month_const_mean <- apply(Month_const, 1, mean)
Month_const_sd <- apply(Month_const, 1, sd)

# # Calcular el N_length basado en talla de las particulas liberadas
# Depth_lengt <- tapply(df$N_length, list(df$ReleaseDepth, df$Year), sum)
# Bathy_lengt <- tapply(df$N_length, list(df$ReleaseBathy, df$Year), sum)
# Zone_lengt  <- tapply(df$N_length, list(df$Zone_name, df$Year), sum)
# Month_lengt <- tapply(df$N_length, list(df$Month, df$Year), sum)
# 
# Depth_lengt <- (Depth_lengt/Depth_all)*100
# Depth_lengt_mean <- apply(Depth_lengt, 1, mean)
# Depth_lengt_sd <- apply(Depth_lengt, 1, sd)
# 
# Bathy_lengt <- (Bathy_lengt/Bathy_all)*100
# Bathy_lengt_mean <- apply(Bathy_lengt, 1, mean)
# Bathy_lengt_sd <- apply(Bathy_lengt, 1, sd)
# 
# Zone_lengt  <- (Zone_lengt/Zone_all)*100
# Zone_lengt_mean <- apply(Zone_lengt, 1, mean)
# Zone_lengt_sd <- apply(Zone_lengt, 1, sd)
# 
# Month_lengt <- (Month_lengt/Month_all)*100
# Month_lengt_mean <- apply(Month_lengt, 1, mean)
# Month_lengt_sd <- apply(Month_lengt, 1, sd)

# Calcular el reclutamiento de ichthyop output
dat <- read.csv(file = paste0(dirpath, 'ichthyop_output.csv'), header = T, sep = ';')

day <- tapply(dat$Recruitprop, list(dat$Month, dat$Year), mean)
day_mean <- apply(day, 1, mean)
day_sd <- apply(day, 1, sd)

depth <- tapply(dat$Recruitprop, list(dat$Depth, dat$Year), mean)
depth_mean <- apply(depth, 1, mean)
depth_sd <- apply(depth, 1, sd)

bathy <- tapply(dat$Recruitprop, list(dat$Bathy, dat$Year), mean)
bathy_mean <- apply(bathy, 1, mean)
bathy_sd <- apply(bathy, 1, sd)

zone  <- tapply(dat$Recruitprop, list(dat$Zone_name, dat$Year), mean)
zone_mean <- apply(zone, 1, mean)
zone_sd <- apply(zone, 1, sd)

latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º-', lats[i] + 2, 'º'))

png(filename = paste0(dirpath, '2_4plots_errorBar.png'), width = 1250, height = 550, res = 120)
par(mfrow = c(2,4), mar = c(4,3,1,.1))

# Plot de no mortalidad
ylim <- c(0,60)

plotbar <- barplot(day_mean, ylim = ylim); mtext(text = 'Spawning Month', side = 1, line = 2, cex = 0.75)
mtext(side = 3, adj = .95, line = -1, text = 'No mortality', cex = .8)
mtext(side = 2, line = 2, cex = 0.75, font = 2, text = ylab)

arrows(plotbar, day_mean + day_sd,
       plotbar, day_mean - day_sd,
       angle=90,code=3,length=0.025)

plotbar <- barplot(depth_mean, ylim = ylim); mtext(text = 'Spawning Depth', side = 1, line = 2, cex = 0.75)
arrows(plotbar, depth_mean + depth_sd,
       plotbar, depth_mean - depth_sd,
       angle=90,code=3,length=0.025)

plotbar <- barplot(bathy_mean, ylim = ylim); mtext(text = 'Spawning Bathymetry', side = 1, line = 2, cex = 0.75)
arrows(plotbar, bathy_mean + bathy_sd,
       plotbar, bathy_mean - bathy_sd,
       angle=90,code=3,length=0.025)

plotbar <- barplot(zone_mean, ylim = ylim, names.arg = F); mtext(text = 'Spawning Latitude', side = 1, line = 2, cex = 0.75)
arrows(plotbar, zone_mean + zone_sd,
       plotbar, zone_mean - zone_sd,
       angle=90,code=3,length=0.025)
text(plotbar, par('usr')[3], labels = latlab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = .85)

# Plot mortalidad constante
ylim <- c(0,5)
plotbar <- barplot(Month_const_mean, ylim = ylim); mtext(text = 'Spawning Month', side = 1, line = 2, cex = 0.75)
mtext(side = 3, adj = .95, line = -1, text = 'Constant mortality', cex = .8)
mtext(side = 2, line = 2, cex = 0.75, font = 2, text = ylab)

arrows(plotbar, Month_const_mean + Month_const_sd,
       plotbar, Month_const_mean - Month_const_sd,
       angle=90,code=3,length=0.025)

plotbar <- barplot(Depth_const_mean, ylim = ylim); mtext(text = 'Spawning Depth', side = 1, line = 2, cex = 0.75)
arrows(plotbar, Depth_const_mean + Depth_const_sd,
       plotbar, Depth_const_mean - Depth_const_sd,
       angle=90,code=3,length=0.025)

plotbar <- barplot(Bathy_const_mean, ylim = ylim); mtext(text = 'Spawning Bathymetry', side = 1, line = 2, cex = 0.75)
arrows(plotbar, Bathy_const_mean + Bathy_const_sd,
       plotbar, Bathy_const_mean - Bathy_const_sd,
       angle=90,code=3,length=0.025)

plotbar <- barplot(Zone_const_mean, ylim = ylim, names.arg = F); mtext(text = 'Spawning Latitude', side = 1, line = 2, cex = 0.75)
arrows(plotbar, Zone_const_mean + Zone_const_sd,
       plotbar, Zone_const_mean - Zone_const_sd,
       angle=90,code=3,length=0.025)
text(plotbar, par('usr')[3], labels = latlab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = .85)

# ylim = c(0,2)
# 
# barplot(Month_lengt, ylim = ylim); mtext(text = 'Spawning Month', side = 1, line = 2, cex = 0.75); mtext(side = 3, adj = .1, line = -1, text = 'Length-dependent mortality', cex = .8)
# barplot(Depth_lengt, ylim = ylim); mtext(text = 'Spawning Depth', side = 1, line = 2, cex = 0.75)
# barplot(Bathy_lengt, ylim = ylim); mtext(text = 'Spawning Bathymetry', side = 1, line = 2, cex = 0.75)
# barplot(Zone_lengt, ylim = ylim, names.arg = F); mtext(text = 'Spawning Latitude', side = 1, line = 2, cex = 0.75)
# text(p1, par('usr')[3], labels = latlab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = .85)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#