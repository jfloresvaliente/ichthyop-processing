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

dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEBf1/k_x0_90days/out/results/'
costa <- readMat('C:/Users/jflores/Documents/MATLAB/Matlab_packages/Roms_tools/Run/coastline_l.mat')$ncst
# costa <- readMat('C:/Users/jflores/Documents/MATLAB/Matlab_packages/Roms_tools/Run/coastline_h.mat')$ncst

xlim <- c(-90,-70)
ylim <- c(-20, 5)
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
load(paste0(dirpath, 'data_atRecruitmentAge.Rdata'))
costa <- subset(costa, costa[,1] >= xlim[1] & costa[,1] <= xlim[2] & costa[,2] >= ylim[1] & costa[,2] <= ylim[2])

# Calcular la distancia al reclutamiento
dat <- df[,c(3,4)]
colnames(dat) <- c('lon','lat')

distance <- distm(dat, costa, fun = distGeo)/1000
distance <- apply(X = distance, MARGIN = 1, FUN = min, na.rm = T)

df$RecruitedDistanceCoast <- distance
save(df, file = paste0(dirpath, 'data_atRecruitmentAge.Rdata'))
#=============================================================================#
# END OF PROGRAM
#=============================================================================#