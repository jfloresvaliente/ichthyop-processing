#=============================================================================#
# Name   : get_hovmuller_latitude_tx
# Author : 
# Date   : 
# Version:
# Aim    : Get Hovmuller matrix of recruitment at higher spatial (spawning latitude) and temporal (spawning frequency) resolution.
# URL    : 
#=============================================================================#
dirpath       <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/10kmparent/FISICA/out/results/'
latilim       <- c(-20, -2)   # Latitude extension of the spawning zone
lat_div       <- .1          # Latitudinal resolution
computeattime <- 61           # Step time to calculate larval retention
year          <- 3            # Number of years
t_x           <- c(1,4,7)

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dir.create(path = paste0(dirpath, 'hovmuller'), showWarnings = F)
hovmullerRdata <- paste0(dirpath, 'hovmuller/hovmuller', lat_div, '.Rdata')

lat_ini <- seq(latilim[1], latilim[2], lat_div)
lat_out <- lat_ini + lat_div

hov_release <- NULL
hov_recruit <- NULL
for(j in 1:12){
  Rdata <- paste0(dirpath,'trajectoriesM',j,'.Rdata')
  load(Rdata)
  print(Rdata)
  dat <- trajectories; rm(trajectories)

  tx_release <- NULL
  tx_recruit <- NULL
  for(k in t_x){
    sub_tx <- subset(dat, dat$t_x == k)

      lat_release <- NULL
      lat_recruit <- NULL
      for(i in 1:(length(lat_ini)-1)){
        lat_sub <- subset(sub_tx, sub_tx$Lat >= lat_ini[i] & sub_tx$Lat < lat_out[i] & sub_tx$Timer == 1)

        released  <- dim(lat_sub)[1]
        recruited <- dim(subset(sub_tx, sub_tx$Drifter %in% lat_sub$Drifter & sub_tx$Timer == computeattime & sub_tx$IfRecruited == 1))[1]

        lat_release <- c(lat_release, released)
        lat_recruit <- c(lat_recruit, recruited)
      }
      tx_release <- cbind(tx_release, lat_release)
      tx_recruit <- cbind(tx_recruit, lat_recruit)
  }
  hov_release <- cbind(hov_release, tx_release)
  hov_recruit <- cbind(hov_recruit, tx_recruit)
}

hovmuller <- (hov_recruit/hov_release)*100
hovmuller <- t(hovmuller)
rownames(hovmuller) <- NULL

z <- hovmuller
x <- seq(from = 1, to = 12, length.out = 12*year)
y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])

hovmuller <- list(x,y,z)
names(hovmuller) <- c('x', 'y', 'z')
save(hovmuller, file = hovmullerRdata)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#