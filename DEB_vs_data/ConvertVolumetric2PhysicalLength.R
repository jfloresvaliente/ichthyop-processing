#=============================================================================#
# Name   : ConvertVolumetric2PhysicalLength
# Author : Jorge Flores-Valiente
# Date   :
# Version:
# Aim    : To convert VOLUMETRIC LENGTH into PHYSICAL LENGTH
# URL    :
#=============================================================================#

# Shape coefficient
del_M <- 0.1889


#------------- Transform VOLUMETRIC LENGTH to PHYSICAL LENGTH -------------#
# Volumetric length (V)
V  <- 0.1038

# Physical length (Lw)
Lw <- V^(1/3)/del_M

print(paste('Volumetric length (V) = ', V))
print(paste('Physical length (cm) = ', Lw))
#=============================================================================#
# END OF PROGRAM
#=============================================================================#