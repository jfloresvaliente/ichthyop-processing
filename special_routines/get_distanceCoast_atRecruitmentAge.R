#=============================================================================#
# Name   : get_distanceCoast_atRecruitmentAge
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Obtener la distancia a la costa al momento del reclutamiento
# URL    : 
#=============================================================================#
library(geosphere)
library(R.matlab)

dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEBf1/k_x0/out/results/'
costa   <- readMat('C:/Users/jflores/Documents/MATLAB/Matlab_packages/Roms_tools/Run/coastline_h.mat')$ncst

xlim <- c(-90,-70)
ylim <- c(-20, 0)
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
load(paste0(dirpath, 'data_atRecruitmentAge.Rdata'))
costa <- subset(costa, costa[,1] >= xlim[1] & costa[,1] <= xlim[2] & costa[,2] >= ylim[1] & costa[,2] <= ylim[2])

# Calcular la distancia al reclutamiento
dis_vec <- numeric(length = dim(df)[1])
for(i in 1:dim(df)[1]){
  dat <- as.matrix(df[i,c(3,4)]) # Colums 3 & 4 (final position LON-LAT)
  dis <- min(distm(x = dat, y = costa, fun = distGeo)/1000)
  dis_vec[i] <- dis
  print(i)
}

df$RecruitedDistanceCoast <- dis_vec
save(df, file = paste0(dirpath, 'data_atRecruitmentAge.Rdata'))
#=============================================================================#
# END OF PROGRAM
#=============================================================================#
