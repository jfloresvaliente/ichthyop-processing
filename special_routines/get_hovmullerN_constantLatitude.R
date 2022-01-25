#=============================================================================#
# Name   : get_hovmullerN_constantLatitude
# Author : 
# Date   : 
# Version:
# Aim    : Get Hovmuller matrix of recruitment at higher spatial (spawning latitude) and temporal (spawning frequency) resolution.
# URL    : 
#=============================================================================#
# dirpath       <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC1/out25C/results_no_shelf/'
latilim       <- c(-20, -2) # Latitude extension of the spawning zone
lat_div       <- 0.5          # Latitudinal resolution
year          <- 2012:2014  # Number of years
t_x           <- c(1,4,7)

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dir.create(path = paste0(dirpath, '/hovmuller/'), showWarnings = F)
hovmullerRdata <- paste0(dirpath, '/hovmuller/hovmullerN_constant', lat_div, 'degrees.Rdata')

lat_ini <- seq(latilim[1], latilim[2], lat_div)
lat_out <- lat_ini + lat_div

load(file = paste0(dirpath, 'data_atRecruitmentAge.Rdata'))

hovmuller <- NULL
for(i in year){
  for(j in 1:12){
    sub_df <- subset(df, df$Year == i & df$Month == j)
    
    lat_release <- NULL
    lat_recruit <- NULL
    for(k in 1:(length(lat_ini)-1)){
      
      lat_sub  <- subset(sub_df, sub_df$Lat_ini >= lat_ini[k] & sub_df$Lat_ini < lat_out[k])
      released <- dim(lat_sub)[1]
      recruite <- sum(subset(lat_sub, lat_sub$IfRecruited == 1)$N_constant)
      
      if(is.na(recruite)) recruite = 0
      
      lat_release <- c(lat_release, released)
      lat_recruit <- c(lat_recruit, recruite)
    }
    hov <- lat_recruit /lat_release * 100
    hovmuller <- cbind(hovmuller, hov)
  }
}

z <- t(hovmuller)
x <- round(seq(from = 1, to = length(year), length.out = dim(z)[1]), 3)
y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])
rownames(z) <- x
colnames(z) <- y

hovmuller <- list(x = x, y = y, z = z)
save(hovmuller, file = hovmullerRdata)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#