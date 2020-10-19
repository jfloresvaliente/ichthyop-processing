library(fields)
library(maps)
library(mapdata)
source('source/plot_traj_ggmap_depth.R')

dirpath   <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/MESO60dias/results_DEB/'
new_path  <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/cfg/'
xy              <- read.table(paste0(new_path, 'peru_drifters.csv'), sep = ';')

M <- 1
# zone <- c(-14, -8)
zone <- c(-10, -8)

load(paste0(dirpath, 'trajectoriesM',M,'.Rdata'))
dat <- trajectories; rm(trajectories)


pixe <- levels(factor(xy$V3))[1:3]
for(i in 1:length(pixe)){
  pngfile <- paste0(dirpath, 'M',M,'_', pixe[i], '10-8.png')
  
  index <- subset(xy, xy$V2 >= zone[1] & xy$V2 < zone[2] & xy$V3 == pixe[i])
  drift <- subset(dat, dat$Timer == 1)
  
  drif_index <- NULL
  for(j in 1:dim(index)[1]){
    drif_index <- c(drif_index, which(round(drift$Lon,2) == round(index$V1[j],2) & round(drift$Lat,2) == round(index$V2[j],2)))
  }
  drift <- subset(dat, dat$Drifter %in% drif_index & dat$IfRecruited == 1)
  # depth <- subset(drift, drift$Timer == 1 & drift$Depth == -30)
  # drift <- subset(drift, drift$Drifter %in% depth$Drifter)
  plot_traj_ggmap_depth(df = drift, pngfile = pngfile, xlim = c(-81, -77), ylim = c(-12,-7),
                        title = paste('Release distance to the coast (', as.numeric(pixe[i]) * 10, 'km )'))
}

