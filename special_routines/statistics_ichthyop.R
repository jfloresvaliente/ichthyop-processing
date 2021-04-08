#===============================================================================
# Name   : statistics_ichthyop
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : make a lm and anova for ouputs of ichthyop simulations 
# URL    : 
#===============================================================================
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB/k_x1.6_90days/out/results/'
N0        <- 1 # Initial value of the particle affected by mortality
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
# dat <- read.table(paste0(dirpath,'ichthyop_output.csv'), header = T, sep = ';')

dat <- read.table(paste0(dirpath,'ichthyop_output_mortality.csv'), header = T, sep = ';')
# Convertir N_length y N_constant en porcentajes
dat$N_length   <- (dat$N_length*100)  /(dat$NumberReleased*N0)
dat$N_constant <- (dat$N_constant*100)/(dat$NumberReleased*N0)

# #====== lm  sin mortalidad======#
# mod <- lm(data = dat, Recruitprop ~
#             factor(Year) + factor(Month) + factor(Depth) + factor(Bathy) + factor(Zone_name)
#           + factor(Year):factor(Month) + factor(Year):factor(Depth) + factor(Year):factor(Bathy) + factor(Year):factor(Zone_name)
#           + factor(Month):factor(Depth) + factor(Month):factor(Bathy) + factor(Month):factor(Zone_name)
#           + factor(Depth):factor(Bathy) + factor(Depth):factor(Zone_name)
#           + factor(Bathy):factor(Zone_name)
#           )

#====== lm  con mortalidad======#
mod <- lm(data = dat, N_constant ~
            factor(Year) + factor(Month) + factor(Depth) + factor(Bathy) + factor(Zone_name)
          + factor(Year):factor(Month) + factor(Year):factor(Depth) + factor(Year):factor(Bathy) + factor(Year):factor(Zone_name)
          + factor(Month):factor(Depth) + factor(Month):factor(Bathy) + factor(Month):factor(Zone_name)
          + factor(Depth):factor(Bathy) + factor(Depth):factor(Zone_name)
          + factor(Bathy):factor(Zone_name)
)

aov = anova(mod)
print(aov)

aov_sum <- (100 * aov[2] / sum(aov[2])); colnames(aov_sum) <- '%Exp'

aov <- cbind(aov,aov_sum)
rownames(aov) <- c('Year','Month','Depth','Bathymetry','Latitude',
                   'Year x Month','Year x Depth','Year x Bathymetry','Year x Latitude',
                   'Month x Depth','Month x Bathymetry','Month x Latitude',
                   'Depth x Bathymetry','Depth x Latitude',
                   'Bathymetry x Latitude','Residuals')
                   

write.table(as.matrix(aov),     file = paste0(dirpath,'ANOVA.csv'), na = '', sep = ';')
# write.table(as.matrix(aov_sum), file = paste0(dirpath,'ANOVA_SUM.csv'), na = '', sep = ';')

#=============================================================================#
# END OF PROGRAM
#=============================================================================#