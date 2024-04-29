#=============================================================================#
# Name   : ConvertPhysical2VolumetricLength
# Author : Jorge Flores-Valiente
# Date   :
# Version:
# Aim    : To convert PHYSICAL LENGTH into VOLUMETRIC LENGTH and vice versa.
# URL    :
#=============================================================================#

# Shape coefficient
del_M <- 0.1889


#------------- Transform VOLUMETRIC LENGTH to PHYSICAL LENGTH -------------#
# Physical length (Lw)
Lw <- 2.48790519286995

# Volumetric length (V)
V  <- (Lw*del_M)^3

print(paste('Physical length (cm) = ', Lw))
print(paste('Volumetric length (V) = ', V))
#=============================================================================#
# END OF PROGRAM
#=============================================================================#