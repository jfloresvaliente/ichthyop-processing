#===============================================================================
# Name   : statistics_ichthyop
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : make a lm and anova for ouputs of ichthyop simulations 
# URL    : 
#===============================================================================
dirpath <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/10kmparent/DEBf1/k_x0_90days/out/results/'

dat <- read.table(paste0(dirpath,'ichthyop_output.csv'), header = T, sep = ';')

#====== lm ======#
mod <- lm(data = dat, Recruitprop ~
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
write.table(as.matrix(aov_sum), file = paste0(dirpath,'ANOVA_SUM.csv'), na = '', sep = ';')

