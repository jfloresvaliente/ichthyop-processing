#===============================================================================
# Name   : statistics_ichthyop
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : make a lm and anova for ouputs of ichthyop simulations 
# URL    : 
#===============================================================================
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052/case2/results/'

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

dat <- read.table(paste0(dirpath,'ichthyop_output2.csv'), header = T, sep = ';')

#====== lm  sin mortalidad======#
mod <- lm(data = dat, Recruitprop ~
            factor(Year) + factor(Month) + factor(ReleaseDepth) + factor(ReleaseBathy) + factor(Zone_name)
          + factor(Year):factor(Month) + factor(Year):factor(ReleaseDepth) + factor(Year):factor(ReleaseBathy) + factor(Year):factor(Zone_name)
          + factor(Month):factor(ReleaseDepth) + factor(Month):factor(ReleaseBathy) + factor(Month):factor(Zone_name)
          + factor(ReleaseDepth):factor(ReleaseBathy) + factor(ReleaseDepth):factor(Zone_name)
          + factor(ReleaseBathy):factor(Zone_name)
          )

aov = anova(mod)
aov_sum <- (100 * aov[2] / sum(aov[2])); colnames(aov_sum) <- '%Exp'
aov <- cbind(aov,aov_sum)

rownames(aov) <- c('Year','Month','Depth','Bathymetry','Latitude',
                   'Year x Month','Year x Depth','Year x Bathymetry','Year x Latitude',
                   'Month x Depth','Month x Bathymetry','Month x Latitude',
                   'Depth x Bathymetry','Depth x Latitude',
                   'Bathymetry x Latitude','Residuals')

write.table(as.matrix(aov), file = paste0(dirpath,'ANOVA_Recruitprop.csv'), na = '', sep = ';')

#====== lm  con mortalidad======#
mod <- lm(data = dat, N_constantprop ~
            factor(Year) + factor(Month) + factor(ReleaseDepth) + factor(ReleaseBathy) + factor(Zone_name)
          + factor(Year):factor(Month) + factor(Year):factor(ReleaseDepth) + factor(Year):factor(ReleaseBathy) + factor(Year):factor(Zone_name)
          + factor(Month):factor(ReleaseDepth) + factor(Month):factor(ReleaseBathy) + factor(Month):factor(Zone_name)
          + factor(ReleaseDepth):factor(ReleaseBathy) + factor(ReleaseDepth):factor(Zone_name)
          + factor(ReleaseBathy):factor(Zone_name)
)

aov = anova(mod)
aov_sum <- (100 * aov[2] / sum(aov[2])); colnames(aov_sum) <- '%Exp'
aov <- cbind(aov,aov_sum)
rownames(aov) <- c('Year','Month','Depth','Bathymetry','Latitude',
                   'Year x Month','Year x Depth','Year x Bathymetry','Year x Latitude',
                   'Month x Depth','Month x Bathymetry','Month x Latitude',
                   'Depth x Bathymetry','Depth x Latitude',
                   'Bathymetry x Latitude','Residuals')
                   
write.table(as.matrix(aov), file = paste0(dirpath,'ANOVA_N_constantprop.csv'), na = '', sep = ';')
#=============================================================================#
# END OF PROGRAM
#=============================================================================#